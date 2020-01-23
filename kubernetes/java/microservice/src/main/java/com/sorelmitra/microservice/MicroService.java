package com.sorelmitra.microservice;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;

@Component
public class MicroService {
    private List<WorkHorse> workHorses;

    public MicroService() {
        this.workHorses = new LinkedList<WorkHorse>();
        addWorkHorse(new WorkHorse(Constants.WORK_HORSE_1, Constants.SLEEP_TIME_MS_1, Constants.NO_CRASH));
        addWorkHorse(new WorkHorse(Constants.WORK_HORSE_2, Constants.SLEEP_TIME_MS_2, Constants.TICKS_BEFORE_CRASH));
    }

    public void run() {
        workHorses.forEach(workHorse -> workHorse.start());
    }

    private void addWorkHorse(WorkHorse workHorse) {
        workHorses.add(workHorse);
    }

    public boolean areAllThreadsAlive() {
        AtomicBoolean alive = new AtomicBoolean(true);
        workHorses.forEach(workHorse -> {
            if (!workHorse.isAlive()) {
                alive.set(false);
            }
        });
        return alive.get();
    }
}
