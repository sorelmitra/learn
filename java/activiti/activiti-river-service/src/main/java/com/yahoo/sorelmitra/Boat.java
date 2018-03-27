package com.yahoo.sorelmitra;

import java.io.Serializable;

import org.apache.commons.lang3.builder.HashCodeBuilder;

public class Boat implements Serializable {

    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    private String name;
    private int seats;
    private int emptySeats;

    public Boat(String name, int seats) {
        this.name = name;
        this.seats = seats;
        this.emptySeats = seats;
    }

    @Override
    public String toString() {
        return "Boat " + getName() + ": " + seats + " seats, " + emptySeats + " empty";
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null) {
            return false;
        }
        if (!(obj instanceof Boat)) {
            return false;
        }
        Boat other = (Boat) obj;
        return (getName() == other.getName()) && (seats == other.seats) && (emptySeats == other.emptySeats);
    }

    @Override
    public int hashCode() {
        HashCodeBuilder hb = new HashCodeBuilder(13, 25);
        hb.append(getName());
        hb.append(seats);
        hb.append(emptySeats);
        return hb.hashCode();
    }

    public int getSeats() {
        return seats;
    }

    public void setSeats(int seats) {
        this.seats = seats;
    }

    public int getEmptySeats() {
        return emptySeats;
    }

    public void setEmptySeats(int emptySeats) {
        this.emptySeats = emptySeats;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public void embarkTourists(int tourists) throws RiverServiceException {
        if (tourists > seats) {
            throw new RiverServiceException(
                    "Not enough seats (" + seats + ") in the boat to accomodate all tourists (" + tourists + ")");
        }
        if (tourists > emptySeats) {
            throw new RiverServiceException("Not enough empty seats (" + emptySeats
                    + ") in the boat to accomodate all tourists (" + tourists + ")");
        }
        emptySeats -= tourists;
    }

}
