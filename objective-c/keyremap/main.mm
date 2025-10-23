// Build as Objective-C++ (.mm)
#include <ApplicationServices/ApplicationServices.h>
#import <AppKit/AppKit.h>

#include <unordered_map>
#include <vector>
#include <optional>
#include <string>
#include <iostream>
#include <algorithm>
#include <unistd.h>

// ---------- Key combo + rule types ----------

struct KeyCombo {
    CGEventFlags mods;                         // required modifiers to match
    std::optional<CGKeyCode> key;              // optional key: nullopt means "any key"

    bool operator==(KeyCombo const& o) const {
        return mods == o.mods && key == o.key;
    }
};

namespace std {
    template<> struct hash<KeyCombo> {
        size_t operator()(KeyCombo const& kc) const noexcept {
            uint64_t key_part = kc.key.has_value() ? static_cast<uint64_t>(*kc.key) : 0xFFFFFFFFull;
            return std::hash<uint64_t>()((uint64_t)kc.mods) ^ (std::hash<uint64_t>()(key_part) << 1);
        }
    };
}

// A remap rule: from -> to, optionally scoped to one or more bundle ids.
// - If bundle_ids has a value, the rule applies only to those apps.
// - If bundle_ids is std::nullopt, the rule is global.
// - If from.key == nullopt, match ANY key pressed with exactly these modifiers.
// - If to.key   == nullopt, preserve the original key code (modifier-only remap).
struct RemapRule {
    KeyCombo from;
    KeyCombo to;
    std::optional<std::vector<std::string>> bundle_ids;
};

static std::vector<RemapRule> g_rules = {
    // Change Slack annoying shortcuts (support both Slack bundle ids)
    {
        { kCGEventFlagMaskCommand, (CGKeyCode)0x23 /* P */ },
        { kCGEventFlagMaskCommand, (CGKeyCode)0x28 /* K */ },
        std::optional<std::vector<std::string>>{ { "com.tinyspeck.slackmacgap", "com.slack.Slack" } }
    },
    {
        { kCGEventFlagMaskCommand, (CGKeyCode)0x28 /* K */ },
        { kCGEventFlagMaskCommand | kCGEventFlagMaskShift, (CGKeyCode)0x20 /* U */ },
        std::optional<std::vector<std::string>>{ { "com.tinyspeck.slackmacgap", "com.slack.Slack" } }
    },

    // Global modifier changes
    {
        { kCGEventFlagMaskCommand, std::nullopt },
        { kCGEventFlagMaskAlternate, std::nullopt },
        std::nullopt
    },
    {
        { kCGEventFlagMaskAlternate, std::nullopt },
        { kCGEventFlagMaskCommand, std::nullopt },
        std::nullopt
    },
};

// ---------- Frontmost app bundle id ----------

static std::string frontBundleID() {
    NSRunningApplication *app = [[NSWorkspace sharedWorkspace] frontmostApplication];
    if (!app) return std::string();
    NSString *bid = app.bundleIdentifier;
    if (!bid) return std::string();
    return std::string([bid UTF8String]);
}

static inline bool bundleListMatches(const std::optional<std::vector<std::string>>& list,
                                     const std::string& current) {
    if (!list) return false;
    const auto& v = *list;
    return std::find(v.begin(), v.end(), current) != v.end();
}

static inline bool keyMatches(const std::optional<CGKeyCode>& pattern, CGKeyCode incoming) {
    return !pattern.has_value() || *pattern == incoming; // nullopt matches any key
}

// ---------- Event tap callback ----------

static CGEventRef eventTapCallback(CGEventTapProxy proxy, CGEventType type, CGEventRef event, void *) {
    if (type != kCGEventKeyDown && type != kCGEventKeyUp) {
        return event;
    }

    // Ignore events we injected ourselves
    pid_t srcpid = (pid_t)CGEventGetIntegerValueField(event, kCGEventSourceUnixProcessID);
    if (srcpid == getpid()) {
        return event;
    }

    // Normalize relevant modifier flags
    CGEventFlags mods = CGEventGetFlags(event) & (
        kCGEventFlagMaskShift |
        kCGEventFlagMaskControl |
        kCGEventFlagMaskAlternate |
        kCGEventFlagMaskCommand
    );
    CGKeyCode key = (CGKeyCode)CGEventGetIntegerValueField(event, kCGKeyboardEventKeycode);

    // Compute current front bundle for app-specific matching
    const std::string currentBundle = frontBundleID();

    auto applyRule = [&](const RemapRule& r) -> CGEventRef {
        // Determine outgoing key (preserve if to.key is nullopt)
        CGKeyCode outKey = r.to.key.has_value() ? *r.to.key : key;
        CGEventRef newEvt = CGEventCreateKeyboardEvent(nullptr, outKey, (type == kCGEventKeyDown));
        CGEventSetFlags(newEvt, r.to.mods);
        CGEventTapPostEvent(proxy, newEvt);
        CFRelease(newEvt);
        return nullptr; // suppress original
    };

    // 1) Try app-specific rules for the current bundle id
    for (const auto& r : g_rules) {
        if (bundleListMatches(r.bundle_ids, currentBundle) &&
            r.from.mods == mods &&
            keyMatches(r.from.key, key)) {
            return applyRule(r);
        }
    }

    // 2) Fallback to global rules (bundle_ids == nullopt)
    for (const auto& r : g_rules) {
        if (!r.bundle_ids &&
            r.from.mods == mods &&
            keyMatches(r.from.key, key)) {
            return applyRule(r);
        }
    }

    return event;
}

// ---------- Main ----------

int main() {
    std::cout << "Starting key-remapper (session tap)…\n";

    // Session-level tap (does not require root like HID tap often does)
    CFMachPortRef eventTap = CGEventTapCreate(
        kCGSessionEventTap,
        kCGTailAppendEventTap,
        kCGEventTapOptionDefault,
        CGEventMaskBit(kCGEventKeyDown) | CGEventMaskBit(kCGEventKeyUp),
        eventTapCallback,
        nullptr
    );
    if (!eventTap) {
        std::cerr << "Failed to create event tap. Check Accessibility + Input Monitoring permissions.\n";
        return 1;
    }

    CFRunLoopSourceRef src = CFMachPortCreateRunLoopSource(kCFAllocatorDefault, eventTap, 0);
    CFRunLoopAddSource(CFRunLoopGetCurrent(), src, kCFRunLoopCommonModes);
    CGEventTapEnable(eventTap, true);

    std::cout << "Remapping active. Ctrl+C to quit.\n";
    CFRunLoopRun();

    CFRelease(src);
    CFRelease(eventTap);
    return 0;
}
