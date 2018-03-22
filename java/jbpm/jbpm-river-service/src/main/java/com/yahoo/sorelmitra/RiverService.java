package com.yahoo.sorelmitra;

import java.util.LinkedList;
import java.util.List;

import org.kie.api.runtime.KieSession;
import org.kie.api.runtime.process.ProcessInstance;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class RiverService {
    private static Logger LOG = LoggerFactory.getLogger(RiverService.class);

    private List<Boat> boats;
    private RiverServiceFactory kieFactory;
    private ProcessInstance processInstance;
    private KieSession ksession;
    boolean embarked;

    public RiverService() {
        boats = new LinkedList<Boat>();
        embarked = false;
    }

    public void addBoat(Boat b) {
        boats.add(b);
    }

    public boolean embarkTourists(int n) throws RiverServiceException {
        processInstance = ksession.startProcess("com.yahoo.sorelmitra.EmbarkTourists");
        int state = processInstance.getState();
        LOG.info("Process state is " + state);
        if (state != ProcessInstance.STATE_COMPLETED) {
            throw new RiverServiceException("Process state is not COMPLETED, but " + state);
        }
        return embarked;
    }

    public RiverServiceFactory getKieFactory() {
        return kieFactory;
    }

    public void setKieFactory(RiverServiceFactory kieFactory) {
        this.kieFactory = kieFactory;
        ksession = kieFactory.getKieSession();
    }

}
