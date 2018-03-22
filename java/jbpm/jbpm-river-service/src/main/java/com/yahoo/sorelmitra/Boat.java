package com.yahoo.sorelmitra;

public class Boat {

    private int seats;
    private int emptySeats;

    public Boat(int seats) {
        this.seats = seats;
        this.emptySeats = seats;
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
