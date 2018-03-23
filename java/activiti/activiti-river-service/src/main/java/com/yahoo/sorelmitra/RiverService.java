package com.yahoo.sorelmitra;

import org.activiti.engine.delegate.DelegateExecution;
import org.activiti.engine.delegate.JavaDelegate;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

@Component
public class RiverService implements JavaDelegate {
    private static Logger LOG = LoggerFactory.getLogger(RiverService.class);

    public RiverService() {
        LOG.info("Constructed");
    }

    @Override
    public void execute(DelegateExecution arg0) {
        getBoatStatus();
    }

    private void getBoatStatus() {
        LOG.info("Getting boat status...");
    }

}
