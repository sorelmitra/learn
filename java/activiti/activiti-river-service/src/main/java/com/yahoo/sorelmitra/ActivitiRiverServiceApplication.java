package com.yahoo.sorelmitra;

import java.util.HashMap;
import java.util.Map;

import org.activiti.engine.RepositoryService;
import org.activiti.engine.RuntimeService;
import org.activiti.engine.TaskService;
import org.activiti.engine.runtime.ProcessInstance;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;

@SpringBootApplication
public class ActivitiRiverServiceApplication {
    private static Logger LOG = LoggerFactory.getLogger(ActivitiRiverServiceApplication.class);

    public static void main(String[] args) {
        SpringApplication.run(ActivitiRiverServiceApplication.class, args);
    }

    @Bean
    public CommandLineRunner init(final RepositoryService repositoryService, final RuntimeService runtimeService,
            final TaskService taskService) {

        return new CommandLineRunner() {
            @Override
            public void run(String... strings) throws Exception {
                Map<String, Object> variables = new HashMap<String, Object>();
                ProcessInstance processInstance = runtimeService.startProcessInstanceByKey("embarkTourist", variables);
                LOG.info("Embarking process started with process instance id [" + processInstance.getProcessInstanceId()
                        + "] key [" + processInstance.getProcessDefinitionKey() + "]");
            }
        };

    }

}
