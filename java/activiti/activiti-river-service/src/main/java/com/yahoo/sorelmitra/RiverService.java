package com.yahoo.sorelmitra;

import java.util.Iterator;
import java.util.LinkedList;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

@Component
public class RiverService {
    private static Logger LOG = LoggerFactory.getLogger(RiverService.class);
    private LinkedList<Boat> boats;
    private int tourists;
    private Boat currentBoat;
    private Iterator<Boat> it;
    private boolean noBoatWithEnoughEmptySeats;

    public RiverService() {
        boats = new LinkedList<Boat>();
        tourists = 0;
        LOG.info("Constructed");
    }

    public void init() {
        it = null;
        noBoatWithEnoughEmptySeats = false;
    }

    public boolean getBoatStatus() {
        LOG.info("Getting boat status...");
        if (it == null) {
            it = boats.iterator();
        }
        if (it.hasNext()) {
            return true;
        }
        it = null;
        return false;
    }

    public int getTouristsCount() {
        LOG.info("Getting tourists count...");
        return tourists;
    }

    public int getNextBoat() throws RiverServiceException {
        if (it == null) {
            throw new RiverServiceException("No boats to iterate!");
        }
        if (!it.hasNext()) {
            throw new RiverServiceException("No more boats to iterate!");
        }

        currentBoat = it.next();
        return currentBoat.getEmptySeats();
    }

    public void embarkTouristsInCurrentBoat() throws RiverServiceException {
        currentBoat.embarkTourists(tourists);
        LOG.info("Embarked " + tourists + " in boat " + currentBoat.getName());
        LOG.info("" + currentBoat);
        tourists = 0;
    }

    public void informNoBoatAvailable() {
        noBoatWithEnoughEmptySeats = true;
        LOG.info("No boat was found with enough empty seats to accomodate " + tourists + " tourists");
    }

    public void addBoat(Boat b) {
        boats.add(b);
        LOG.info("Added " + b);
    }

    public void addTourists(int n) {
        tourists += n;
        LOG.info(tourists + " tourists so far");
    }

    public boolean getNoBoatWithEnoughEmptySeats() {
        return noBoatWithEnoughEmptySeats;
    }
}
