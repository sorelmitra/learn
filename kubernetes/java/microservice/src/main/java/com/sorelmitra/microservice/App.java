package com.sorelmitra.microservice;

import java.util.LinkedList;
import java.util.List;

public class App {

    private List<WorkHorse> workHorses;

    public App() {
        this.workHorses = new LinkedList<WorkHorse>();
    }

    public static void main(String[] args) {
        App app = new App();
        app.addWorkHorse(new WorkHorse(Constants.WORK_HORSE_1, Constants.SLEEP_TIME_MS_1, Constants.NO_CRASH));
        app.addWorkHorse(new WorkHorse(Constants.WORK_HORSE_2, Constants.SLEEP_TIME_MS_2, Constants.TICKS_BEFORE_CRASH));
        app.start();
    }

    private void start() {
        workHorses.forEach(workHorse -> workHorse.start());
    }

    private void addWorkHorse(WorkHorse workHorse) {
        workHorses.add(workHorse);
    }
}
