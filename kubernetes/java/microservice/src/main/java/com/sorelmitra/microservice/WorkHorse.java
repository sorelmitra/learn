package com.sorelmitra.microservice;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class WorkHorse implements Runnable {

    private static final Logger LOG = LoggerFactory.getLogger(WorkHorse.class);

    private final Thread t;
    private long sleepTimeMs;
    private String name;
    private boolean stopped;
    private int ticksBeforeCrash;
    private int ticks;

    public WorkHorse(String name, long sleepTimeMs, int ticksBeforeCrash) {
        this.name = name;
        this.sleepTimeMs = sleepTimeMs;
        this.ticksBeforeCrash = ticksBeforeCrash;
        this.stopped = false;
        this.ticks = 0;
        t = new Thread(this);
    }

    public void start() {
        t.start();
    }

    @Override
    public void run() {
        while (!stopped) {
            try {
                Thread.sleep(sleepTimeMs);
                if (shouldCrash()) {
                    throw new NullPointerException(String.format("Crash by design in thread %s", name, ticks));
                }
                LOG.info("Thread {} ticked {}", name, ticks);
            } catch (InterruptedException e) {
                LOG.info("Thread {} ticked - interrupted", name);
            }
        }
    }

    private boolean shouldCrash() {
        ticks++;
        if (ticksBeforeCrash < 1) return false;
        if (ticks <= ticksBeforeCrash) return false;
        ticks = 0;
        return true;
    }

    public boolean isAlive() {
        if (t.isAlive()) return true;
        LOG.warn("{} is dead!", name);
        return false;
    }
}
