package com.yahoo.sorelmitra;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

@Component
public class RiverService {
    private static Logger LOG = LoggerFactory.getLogger(RiverService.class);

    public RiverService() {
        LOG.info("Constructed");
    }

    public void getBoatStatus() {
        LOG.info("Getting boat status...");
    }

}
