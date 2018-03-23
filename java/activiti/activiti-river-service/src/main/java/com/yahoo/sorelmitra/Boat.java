package com.yahoo.sorelmitra;

import java.io.Serializable;

import org.apache.commons.lang3.builder.HashCodeBuilder;

public class Boat implements Serializable {

    private int seats;
    private int emptySeats;

    public Boat(int seats) {
        this.seats = seats;
        this.emptySeats = seats;
    }

    @Override
    public String toString() {
        return "Seats total: " + seats + ", empty: " + emptySeats;
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
        return (seats == other.seats) && (emptySeats == other.emptySeats);
    }

    @Override
    public int hashCode() {
        HashCodeBuilder hb = new HashCodeBuilder(13, 25);
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

}
