package com.sorelmitra.onlinetest;

import java.text.MessageFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

enum LogLevels {
    ERROR,
    WARNING,
    INFO,
    DEBUG,
    TRACE,
}

class LogImpl {
    private LogLevels level;

    public LogImpl(LogLevels level) {
        this.level = level;
    }

    public void error(String fmt, Object ... objects) {
        log(LogLevels.ERROR, fmt, objects);
    }

    public void warn(String fmt, Object ... objects) {
        log(LogLevels.WARNING, fmt, objects);
    }

    public void entry(String fmt, Object ... objects) {
        String message = format(fmt, objects);
        log(LogLevels.INFO, "ENTRY {}", new String[]{message});
    }

    public void info(String fmt, Object ... objects) {
        log(LogLevels.INFO, fmt, objects);
    }

    public void debug(String fmt, Object ... objects) {
        log(LogLevels.DEBUG, fmt, objects);
    }

    public void trace(String fmt, Object ... objects) {
        log(LogLevels.TRACE, fmt, objects);
    }

    private void log(LogLevels desiredLevel, String fmt, Object[] objects) {
        if (level.ordinal() < desiredLevel.ordinal()) {
            return;
        }
        String levelString = desiredLevel.toString();
        String message = format(fmt, objects);
        String annotatedMessage = format("[{}] [{}] [{}] {}", timestamp(), levelString, stackInfo(), message);
        System.out.println(annotatedMessage);
    }

    private String format(String fmt, Object... var2) {
        int i = 0;
        while(fmt.contains("{}")) {
            fmt = fmt.replaceFirst(Pattern.quote("{}"), "{"+ i++ +"}");
        }
        return MessageFormat.format(fmt, var2);
    }

    private String stackInfo() {
        var logStackSize = 4;
        StackWalker instance = StackWalker.getInstance();
        List<StackWalker.StackFrame> lastTwoFrames = instance.walk(frames -> frames
                .limit(logStackSize)
                .collect(Collectors.toList()));
        StackWalker.StackFrame stackFrame = lastTwoFrames.get(logStackSize - 1);
        return format("{}::{}", stackFrame.getClassName(), stackFrame.getMethodName());
    }

    private String timestamp() {
        SimpleDateFormat formatter= new SimpleDateFormat("yyyy-MM-dd 'at' HH:mm:ss z");
        Date date = new Date(System.currentTimeMillis());
        return formatter.format(date);
    }

}

public class Main {

    private LogImpl LOG = new LogImpl(LogLevels.TRACE);

    public int compute(int a, int b) {
        LOG.entry("");
        if (a < 0) {
            LOG.trace("A is negative {}, returning B {}", a, b);
            return b;
        }
        if (b < 0) {
            LOG.trace("B is negative {}, returning A {}", b, a);
            return a;
        }
        LOG.trace("Returning sum {}", a + b);
        return a + b;
    }
}
