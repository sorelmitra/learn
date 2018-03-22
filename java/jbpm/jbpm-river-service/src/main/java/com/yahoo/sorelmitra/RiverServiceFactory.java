package com.yahoo.sorelmitra;

import org.kie.api.KieServices;
import org.kie.api.io.ResourceType;
import org.kie.api.runtime.KieSession;
import org.kie.api.runtime.manager.RuntimeEngine;
import org.kie.api.runtime.manager.RuntimeEnvironment;
import org.kie.api.runtime.manager.RuntimeEnvironmentBuilder;
import org.kie.api.runtime.manager.RuntimeManager;
import org.kie.api.runtime.manager.RuntimeManagerFactory;
import org.kie.internal.io.ResourceFactory;
import org.kie.internal.runtime.manager.context.EmptyContext;

public class RiverServiceFactory {

    private RuntimeManager manager;
    private RuntimeEngine runtimeEngine;
    private RuntimeEnvironment environment;
    private KieSession ksession;

    public RiverServiceFactory() {
        environment = RuntimeEnvironmentBuilder.Factory.get().newEmptyBuilder()
                .knowledgeBase(KieServices.Factory.get().getKieClasspathContainer().getKieBase("kbase"))
                // ONLY REQUIRED FOR PER-REQUEST AND PER-INSTANCE STRATEGY
                // .addEnvironmentEntry("IS_JTA_TRANSACTION", false)
                .addAsset(ResourceFactory.newClassPathResource("EmbarkTourists.bpmn2"), ResourceType.BPMN2)
                .persistence(false).get();

        manager = RuntimeManagerFactory.Factory.get().newSingletonRuntimeManager(environment);

        runtimeEngine = manager.getRuntimeEngine(EmptyContext.get());

        ksession = runtimeEngine.getKieSession();
    }

    public KieSession getKieSession() {
        return ksession;
    }

    public void cleanupKie() {
        manager.disposeRuntimeEngine(runtimeEngine);
    }

}
