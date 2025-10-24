// Build as Objective-C++ (.mm)
// Unified logging: console output is mirrored to ~/Library/Logs/KeyRemap.log
// Disable logging: compile with -DLOG_KEYS=0 or run with KEYREMAP_LOG=0

#ifndef LOG_KEYS
#define LOG_KEYS 1
#endif

#include <ApplicationServices/ApplicationServices.h>
#import <AppKit/AppKit.h>

#include <unordered_map>
#include <vector>
#include <optional>
#include <string>
#include <iostream>
#include <algorithm>
#include <unistd.h>
#include <stdio.h>
#include <time.h>

// ---------- Logging system ----------
static inline bool loggingEnabledRuntime() {
    const char* env = getenv("KEYREMAP_LOG");
    if (!env) return LOG_KEYS != 0;
    return !(env[0] == '0');
}

static FILE* logFileHandle() {
    static FILE* f = nullptr;
    static bool initialized = false;
    if (!initialized) {
        initialized = true;
        if (!loggingEnabledRuntime()) return nullptr;
        @autoreleasepool {
            NSString* path = [NSHomeDirectory() stringByAppendingPathComponent:@"Library/Logs/KeyRemap.log"];
            f = fopen([path fileSystemRepresentation], "a");
            if (f) setvbuf(f, nullptr, _IOLBF, 0); // line-buffered
        }
    }
    return f;
}

static void logLine(const std::string& msg) {
    FILE* f = logFileHandle();
    if (!f) return;
    time_t now = time(nullptr);
    struct tm tmv; localtime_r(&now, &tmv);
    char tbuf[32]; strftime(tbuf, sizeof tbuf, "%Y-%m-%d %H:%M:%S", &tmv);
    fprintf(f, "%s | %s\n", tbuf, msg.c_str());
    fflush(f);
}

static void logPrint(const std::string& msg, bool newline = true) {
    if (newline) std::cout << msg << std::endl; else std::cout << msg;
    logLine(msg);
}

// ---------- Helper formatting ----------
static inline const char* keyEventTypeName(CGEventType t) {
    return (t == kCGEventKeyDown) ? "down" : (t == kCGEventKeyUp ? "up" : "other");
}

static std::string flagsToPretty(CGEventFlags mods) {
    std::string s;
    if (mods & kCGEventFlagMaskCommand)   s += "⌘";
    if (mods & kCGEventFlagMaskAlternate) s += "⌥";
    if (mods & kCGEventFlagMaskControl)   s += "⌃";
    if (mods & kCGEventFlagMaskShift)     s += "⇧";
    if (s.empty()) s = "(none)";
    return s;
}

static const char* mouseEventTypeName(CGEventType t) {
    switch (t) {
        case kCGEventLeftMouseDown:   return "LDown";
        case kCGEventLeftMouseUp:     return "LUp";
        case kCGEventRightMouseDown:  return "RDown";
        case kCGEventRightMouseUp:    return "RUp";
        case kCGEventOtherMouseDown:  return "OtherDown";
        case kCGEventOtherMouseUp:    return "OtherUp";
        case kCGEventScrollWheel:     return "Scroll";
        default:                      return "Mouse";
    }
}

// ---------- Key combo + rule types ----------
struct KeyCombo {
    CGEventFlags mods;
    std::optional<CGKeyCode> key; // nullopt => any key
    bool operator==(KeyCombo const& o) const { return mods == o.mods && key == o.key; }
};

namespace std { template<> struct hash<KeyCombo> { size_t operator()(KeyCombo const& kc) const noexcept {
    uint64_t key_part = kc.key.has_value() ? static_cast<uint64_t>(*kc.key) : 0xFFFFFFFFull;
    return std::hash<uint64_t>()((uint64_t)kc.mods) ^ (std::hash<uint64_t>()(key_part) << 1);
}}; }

struct RemapRule {
    KeyCombo from;
    KeyCombo to;
    std::optional<std::vector<std::string>> bundle_ids; // if omitted => global
};

static std::vector<RemapRule> g_rules = {
    // Slack shortcuts
    { { kCGEventFlagMaskCommand, (CGKeyCode)0x23 /* P */ }, { kCGEventFlagMaskCommand, (CGKeyCode)0x28 /* K */ }, std::optional<std::vector<std::string>>{ { "com.tinyspeck.slackmacgap", "com.slack.Slack" } } },
    { { kCGEventFlagMaskCommand, (CGKeyCode)0x28 /* K */ }, { kCGEventFlagMaskCommand | kCGEventFlagMaskShift, (CGKeyCode)0x20 /* U */ }, std::optional<std::vector<std::string>>{ { "com.tinyspeck.slackmacgap", "com.slack.Slack" } } },

    // Global changes
    {
        { kCGEventFlagMaskCommand, (CGKeyCode)0x72 /* Help / Insert */ },
        { kCGEventFlagMaskCommand, (CGKeyCode)0x73 /* Home */ },
        std::nullopt,
    },
};

// ---------- App context ----------
static std::string frontBundleID() {
    NSRunningApplication *app = [[NSWorkspace sharedWorkspace] frontmostApplication];
    if (!app) return std::string();
    NSString *bid = app.bundleIdentifier; if (!bid) return std::string();
    return std::string([bid UTF8String]);
}

static inline bool bundleListMatches(const std::optional<std::vector<std::string>>& list, const std::string& current) {
    if (!list) return false; // no list => not app-scoped here
    const auto& v = *list;
    return std::find(v.begin(), v.end(), current) != v.end();
}

static inline bool keyMatches(const std::optional<CGKeyCode>& pattern, CGKeyCode incoming) {
    return !pattern.has_value() || *pattern == incoming;
}

// ---------- Logging helpers for events ----------
static void logKeyEvent(const char* phase, CGEventType type, CGKeyCode key, CGEventFlags mods, const std::string& bundle, bool wasRemapped, std::optional<CGKeyCode> outKey, CGEventFlags outMods) {
    if (!loggingEnabledRuntime()) return;
    char buffer[256];
    snprintf(buffer, sizeof(buffer), "%s | %-5s | in:key=0x%02X mods=%s | app=%s%s",
             phase, keyEventTypeName(type), (unsigned)key, flagsToPretty(mods).c_str(), bundle.c_str(),
             wasRemapped ? " | remapped" : "");
    logLine(buffer);
}

static void logMouseEvent(CGEventType type, CGEventRef event) {
    if (!loggingEnabledRuntime()) return;
    CGEventFlags mods = CGEventGetFlags(event) & (kCGEventFlagMaskShift | kCGEventFlagMaskControl | kCGEventFlagMaskAlternate | kCGEventFlagMaskCommand);
    const char* tname = mouseEventTypeName(type);
    if (type == kCGEventScrollWheel) {
        int32_t dy = (int32_t)CGEventGetIntegerValueField(event, kCGScrollWheelEventDeltaAxis1);
        int32_t dx = (int32_t)CGEventGetIntegerValueField(event, kCGScrollWheelEventDeltaAxis2);
        int64_t pdy = CGEventGetIntegerValueField(event, kCGScrollWheelEventPointDeltaAxis1);
        int64_t pdx = CGEventGetIntegerValueField(event, kCGScrollWheelEventPointDeltaAxis2);
        char buf[256];
        snprintf(buf, sizeof(buf), "MOUSE | %s | mods=%s | scroll dy=%d dx=%d (point dy=%lld dx=%lld)", tname, flagsToPretty(mods).c_str(), dy, dx, (long long)pdy, (long long)pdx);
        logLine(buf);
    } else {
        int64_t btn = CGEventGetIntegerValueField(event, kCGMouseEventButtonNumber);
        double x = CGEventGetLocation(event).x;
        double y = CGEventGetLocation(event).y;
        char buf[256];
        snprintf(buf, sizeof(buf), "MOUSE | %s | mods=%s | button=%lld | at(%.0f,%.0f)", tname, flagsToPretty(mods).c_str(), (long long)btn, x, y);
        logLine(buf);
    }
}

// ---------- Event tap callback ----------
static CGEventRef eventTapCallback(CGEventTapProxy proxy, CGEventType type, CGEventRef event, void *) {
    switch (type) {
        case kCGEventKeyDown:
        case kCGEventKeyUp: {
            pid_t srcpid = (pid_t)CGEventGetIntegerValueField(event, kCGEventSourceUnixProcessID);
            if (srcpid == getpid()) return event;
            CGEventFlags mods = CGEventGetFlags(event) & (kCGEventFlagMaskShift | kCGEventFlagMaskControl | kCGEventFlagMaskAlternate | kCGEventFlagMaskCommand);
            CGKeyCode key = (CGKeyCode)CGEventGetIntegerValueField(event, kCGKeyboardEventKeycode);
            const std::string currentBundle = frontBundleID();
            logKeyEvent("RECV", type, key, mods, currentBundle, false, std::nullopt, 0);

            auto applyRule = [&](const RemapRule& r) -> CGEventRef {
                CGKeyCode outKey = r.to.key.has_value() ? *r.to.key : key;
                CGEventRef newEvt = CGEventCreateKeyboardEvent(nullptr, outKey, (type == kCGEventKeyDown));
                CGEventSetFlags(newEvt, r.to.mods);
                CGEventTapPostEvent(proxy, newEvt);
                CFRelease(newEvt);
                logKeyEvent("REMAP", type, key, mods, currentBundle, true, r.to.key, r.to.mods);
                return nullptr;
            };

            for (const auto& r : g_rules) {
                if (bundleListMatches(r.bundle_ids, currentBundle) && r.from.mods == mods && keyMatches(r.from.key, key)) {
                    return applyRule(r);
                }
            }
            for (const auto& r : g_rules) {
                if (!r.bundle_ids && r.from.mods == mods && keyMatches(r.from.key, key)) {
                    return applyRule(r);
                }
            }
            return event;
        }
        case kCGEventLeftMouseDown:
        case kCGEventLeftMouseUp:
        case kCGEventRightMouseDown:
        case kCGEventRightMouseUp:
        case kCGEventOtherMouseDown:
        case kCGEventOtherMouseUp:
        case kCGEventScrollWheel: {
            logMouseEvent(type, event);
            return event; // pass through unchanged
        }
        default:
            return event;
    }
}

// ---------- Main ----------
int main() {
    logPrint("Starting key-remapper (keys + mouse)…");

    CGEventMask mask = 0;
    mask |= CGEventMaskBit(kCGEventKeyDown) | CGEventMaskBit(kCGEventKeyUp);
    mask |= CGEventMaskBit(kCGEventLeftMouseDown) | CGEventMaskBit(kCGEventLeftMouseUp);
    mask |= CGEventMaskBit(kCGEventRightMouseDown) | CGEventMaskBit(kCGEventRightMouseUp);
    mask |= CGEventMaskBit(kCGEventOtherMouseDown) | CGEventMaskBit(kCGEventOtherMouseUp);
    mask |= CGEventMaskBit(kCGEventScrollWheel);

    CFMachPortRef eventTap = CGEventTapCreate(
        kCGSessionEventTap, kCGTailAppendEventTap, kCGEventTapOptionDefault, mask, eventTapCallback, nullptr);
    if (!eventTap) {
        logPrint("Failed to create event tap. Check Accessibility + Input Monitoring permissions.");
        return 1;
    }

    CFRunLoopSourceRef src = CFMachPortCreateRunLoopSource(kCFAllocatorDefault, eventTap, 0);
    CFRunLoopAddSource(CFRunLoopGetCurrent(), src, kCFRunLoopCommonModes);
    CGEventTapEnable(eventTap, true);

    logPrint("Remapping active. Ctrl+C to quit.");
    CFRunLoopRun();

    CFRelease(src);
    CFRelease(eventTap);
    return 0;
}
