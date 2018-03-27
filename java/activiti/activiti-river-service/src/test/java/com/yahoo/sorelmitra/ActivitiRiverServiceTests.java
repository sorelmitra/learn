package com.yahoo.sorelmitra;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.activiti.engine.ProcessEngine;
import org.activiti.engine.ProcessEngineConfiguration;
import org.activiti.engine.RepositoryService;
import org.activiti.engine.RuntimeService;
import org.activiti.engine.impl.cfg.StandaloneProcessEngineConfiguration;
import org.activiti.engine.repository.Deployment;
import org.activiti.engine.repository.ProcessDefinition;
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

    private List<Boat> boats;
    private Boat cutter;

    @Autowired
    private RuntimeService runtimeService;

    private Map<String, Object> variables;

    private TestRiverServiceDelegate riverServiceDelegate;

    @Before
    public void setUp() {
        ProcessEngineConfiguration cfg = new StandaloneProcessEngineConfiguration()
                .setJdbcUrl("jdbc:h2:mem:activiti;DB_CLOSE_DELAY=1000").setJdbcUsername("sa").setJdbcPassword("")
                .setJdbcDriver("org.h2.Driver")
                .setDatabaseSchemaUpdate(ProcessEngineConfiguration.DB_SCHEMA_UPDATE_TRUE);
        ProcessEngine processEngine = cfg.buildProcessEngine();
        String pName = processEngine.getName();
        String ver = ProcessEngine.VERSION;
        LOG.info("ProcessEngine [" + pName + "] Version: [" + ver + "]");

        RepositoryService repositoryService = processEngine.getRepositoryService();
        Deployment deployment = repositoryService.createDeployment()
                .addClasspathResource("processes/EmbarkTourist.bpmn").deploy();
        ProcessDefinition processDefinition = repositoryService.createProcessDefinitionQuery()
                .deploymentId(deployment.getId()).singleResult();
        LOG.info("Found process definition [" + processDefinition.getName() + "] with id [" + processDefinition.getId()
                + "]");

        RuntimeService runtimeService = processEngine.getRuntimeService();

        variables = new HashMap<String, Object>();

        boats = new LinkedList<Boat>();
        cutter = new Boat("Cutty One", 10);
        boats.add(cutter);

        riverServiceDelegate = new TestRiverServiceDelegate();

        variables.put("riverServiceDelegate", riverServiceDelegate);
    }

    @Test
    public void testEmbark() {
        variables.put("boats", boats);
        variables.put("touristsCount", 6);
        ProcessInstance processInstance = runtimeService.startProcessInstanceByKey("embarkTourist", variables);
        LOG.info("Embarking process started with process instance id [" + processInstance.getProcessInstanceId()
                + "] key [" + processInstance.getProcessDefinitionKey() + "]");
        Assert.assertFalse(riverServiceDelegate.isBoatUnavailable());
        Assert.assertEquals(4, cutter.getEmptySeats());

        variables.put("touristsCount", 4);
        processInstance = runtimeService.startProcessInstanceByKey("embarkTourist", variables);
        LOG.info("Embarking process started with process instance id [" + processInstance.getProcessInstanceId()
                + "] key [" + processInstance.getProcessDefinitionKey() + "]");
        Assert.assertFalse(riverServiceDelegate.isBoatUnavailable());
        Assert.assertEquals(0, cutter.getEmptySeats());

        variables.put("touristsCount", 2);
        processInstance = runtimeService.startProcessInstanceByKey("embarkTourist", variables);
        LOG.info("Embarking process started with process instance id [" + processInstance.getProcessInstanceId()
                + "] key [" + processInstance.getProcessDefinitionKey() + "]");
        Assert.assertTrue(riverServiceDelegate.isBoatUnavailable());
        Assert.assertEquals(0, cutter.getEmptySeats());
    }

    @Test
    public void testNoBoat() {
        variables.put("touristsCount", 2);
        ProcessInstance processInstance = runtimeService.startProcessInstanceByKey("embarkTourist", variables);
        LOG.info("Embarking process started with process instance id [" + processInstance.getProcessInstanceId()
                + "] key [" + processInstance.getProcessDefinitionKey() + "]");
        Assert.assertTrue(riverServiceDelegate.isBoatUnavailable());
    }
}
