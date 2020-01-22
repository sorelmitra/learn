package com.sorelmitra.microservice;

import org.springframework.context.annotation.Configuration;
import org.springframework.stereotype.Component;

import java.util.LinkedList;
import java.util.List;

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
}
