package com.yahoo.sorelmitra;

import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class TestRiverServiceDelegate implements RiverServiceDelegate {
    private static Logger LOG = LoggerFactory.getLogger(TestRiverServiceDelegate.class);

    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    private boolean boatUnavailable;

    public TestRiverServiceDelegate() {
        boatUnavailable = false;
    }

    @Override
    public void onBoatUnavailable(Map<String, Object> processInstanceVariables) {
        LOG.info("processInstanceVariables: " + processInstanceVariables);
        setBoatUnavailable(true);
    }

    public boolean isBoatUnavailable() {
        return boatUnavailable;
    }

    public void setBoatUnavailable(boolean boatUnavailable) {
        this.boatUnavailable = boatUnavailable;
    }

}
