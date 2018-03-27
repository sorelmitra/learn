package com.yahoo.sorelmitra;

import java.util.List;

import org.activiti.engine.delegate.DelegateExecution;
import org.activiti.engine.impl.persistence.entity.VariableInstance;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

@Component
public class RiverService {
    private static Logger LOG = LoggerFactory.getLogger(RiverService.class);

    public RiverService() {
        LOG.info("Constructed");
    }

    public void checkTouristsCount(DelegateExecution execution) {
        LOG.info("DelegateExecution: " + execution);
        int t = (Integer) execution.getVariable("touristsCount");
        LOG.info("Checking tourists count: there are " + t + " tourists");
        execution.setVariable("boatsIndex", 0);
    }

    public boolean getBoatStatus(DelegateExecution execution) throws RiverServiceException {
        LOG.info("DelegateExecution: " + execution);
        @SuppressWarnings("unchecked")
        List<Boat> boats = (List<Boat>) execution.getVariable("boats");
        if (boats == null) {
            return false;
        }
        int i = (Integer) execution.getVariable("boatsIndex");
        int n = boats.size();
        int t = (Integer) execution.getVariable("touristsCount");
        LOG.info("Getting boat status: " + n + " boats available, " + t + " tourists are waiting");
        return (i < n);
    }

    public int getNextBoat(DelegateExecution execution) throws RiverServiceException {
        @SuppressWarnings("unchecked")
        List<Boat> boats = (List<Boat>) execution.getVariable("boats");
        int i = (Integer) execution.getVariable("boatsIndex");
        int t = (Integer) execution.getVariable("touristsCount");
        Boat currentBoat = boats.get(i);
        LOG.info("At boat " + i + ": " + currentBoat + ", " + t + " tourists are waiting");
        execution.setVariable("boatsIndex", i + 1);
        execution.setVariable("currentBoat", currentBoat);
        return currentBoat.getEmptySeats();
    }

    public void embarkTouristsInCurrentBoat(DelegateExecution execution) throws RiverServiceException {
        Boat currentBoat = (Boat) execution.getVariable("currentBoat");
        int t = (Integer) execution.getVariable("touristsCount");
        currentBoat.embarkTourists(t);
        LOG.info("Embarked " + t + " in boat " + currentBoat.getName());
        LOG.info("" + currentBoat);
        execution.setVariable("touristsCount", 0);
    }

    public void informNoBoatAvailable(DelegateExecution execution) {
        int t = (Integer) execution.getVariable("touristsCount");
        VariableInstance delegateVariableInstance = execution.getVariableInstance("riverServiceDelegate");
        RiverServiceDelegate delegate = (RiverServiceDelegate) delegateVariableInstance.getValue();
        LOG.info("delegate info: " + delegateVariableInstance);
        LOG.info("No boat was found with enough empty seats to accomodate " + t + " tourists");
        delegate.onBoatUnavailable(execution.getVariables());
    }
}
