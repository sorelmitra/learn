package com.yahoo.sorelmitra;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class RiverServiceTest {

    private RiverServiceFactory factory;
    private RiverService rs;
    private Boat cutter;

    @Before
    public void setUp() {
        factory = new RiverServiceFactory();

        rs = new RiverService();
        rs.setKieFactory(factory);

        cutter = new Boat(10);
        rs.addBoat(cutter);
    }

    @After
    public void cleanUp() {
        factory.cleanupKie();
    }

    @Test
    public void testSingleTourist() throws RiverServiceException {
        Assert.assertEquals(10, cutter.getEmptySeats());

        boolean result;

        result = rs.embarkTourists(6);
        Assert.assertTrue(result);
        Assert.assertEquals(4, cutter.getEmptySeats());

        result = rs.embarkTourists(6);
        Assert.assertFalse(result);
        Assert.assertEquals(4, cutter.getEmptySeats());

        result = rs.embarkTourists(4);
        Assert.assertTrue(result);
        Assert.assertEquals(0, cutter.getEmptySeats());
    }
}
