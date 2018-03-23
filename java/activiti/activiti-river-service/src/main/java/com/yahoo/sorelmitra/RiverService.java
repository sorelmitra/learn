package com.yahoo.sorelmitra;

import java.util.LinkedList;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

@Component
public class RiverService {
    private static Logger LOG = LoggerFactory.getLogger(RiverService.class);
    private LinkedList<Boat> boats;
    private int tourists;

    public RiverService() {
        boats = new LinkedList<Boat>();
        tourists = 0;
        LOG.info("Constructed");
    }

    public boolean getBoatStatus() {
        LOG.info("Getting boat status...");
        return (boats.size() > 0);
    }

    public int getTouristsCount() {
        LOG.info("Getting tourists count...");
        return tourists;
    }

    public void addBoat(Boat b) {
        boats.add(b);
        LOG.info("Added " + b);
    }

    public void addTourists(int n) {
        tourists += n;
        LOG.info(tourists + " tourists so far");
    }

}
