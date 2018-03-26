package com.yahoo.sorelmitra;

import org.activiti.engine.RuntimeService;
import org.activiti.engine.runtime.ProcessInstance;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

@RunWith(SpringJUnit4ClassRunner.class)
@SpringBootTest
public class ActivitiRiverServiceTests {
    private static Logger LOG = LoggerFactory.getLogger(ActivitiRiverServiceTests.class);

    private Boat cutter;

    @Autowired
    private RuntimeService runtimeService;

    @Autowired
    private RiverService riverService;

    @Before
    public void setUp() {
        cutter = new Boat("Cutty One", 10);
    }

    @Test
    public void testEmbark() {
        LOG.info("River service: " + riverService);
        riverService.addBoat(cutter);

        riverService.addTourists(6);
        ProcessInstance processInstance = runtimeService.startProcessInstanceByKey("embarkTourist");
        LOG.info("Embarking process started with process instance id [" + processInstance.getProcessInstanceId()
                + "] key [" + processInstance.getProcessDefinitionKey() + "]");
        Assert.assertEquals(4, cutter.getEmptySeats());

        riverService.addTourists(4);
        processInstance = runtimeService.startProcessInstanceByKey("embarkTourist");
        LOG.info("Embarking process started with process instance id [" + processInstance.getProcessInstanceId()
                + "] key [" + processInstance.getProcessDefinitionKey() + "]");
        Assert.assertEquals(0, cutter.getEmptySeats());

        riverService.addTourists(2);
        processInstance = runtimeService.startProcessInstanceByKey("embarkTourist");
        LOG.info("Embarking process started with process instance id [" + processInstance.getProcessInstanceId()
                + "] key [" + processInstance.getProcessDefinitionKey() + "]");
        Assert.assertTrue(riverService.getNoBoatWithEnoughEmptySeats());
        Assert.assertEquals(0, cutter.getEmptySeats());
    }

    @Test
    public void testNoBoat() {
        riverService.addTourists(2);
        ProcessInstance processInstance = runtimeService.startProcessInstanceByKey("embarkTourist");
        LOG.info("Embarking process started with process instance id [" + processInstance.getProcessInstanceId()
                + "] key [" + processInstance.getProcessDefinitionKey() + "]");
        Assert.assertTrue(riverService.getNoBoatWithEnoughEmptySeats());
    }
}
