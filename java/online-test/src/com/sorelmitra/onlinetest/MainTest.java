package com.sorelmitra.onlinetest;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class MainTest {

    Main m = new Main();

    @Test
    void positiveNumbers() {
        assertEquals(5, m.compute(2, 3));
    }

    @Test
    void negativeFirst() {
        assertEquals(2, m.compute(-1, 3));
    }

    @Test
    void negativeSecond() {
        assertEquals(2, m.compute(2, -1));
    }

}