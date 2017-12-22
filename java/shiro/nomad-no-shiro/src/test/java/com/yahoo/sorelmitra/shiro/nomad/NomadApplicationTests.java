package com.yahoo.sorelmitra.shiro.nomad;

import java.util.List;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

/*
   Base class, should not be ran by itself.
   Instead, inherit an empty class from it and set these annotations:
   @RunWith(SpringRunner.class)
   @SpringBootTest
   @ActiveProfiles(profiles = "<profile-name>")
 */
@Ignore
public class NomadApplicationTests {

	private static Logger LOG = LoggerFactory.getLogger(NomadApplicationTests.class);

	@Autowired
	private NomadSessionFactory sessionFactory;

	@Autowired
	private NomadRepository repository;

	private NomadSession session;

	@Before
	public void setUp() {
		session = createSession();
	}

	@After
	public void tearDown() {
	}

	@Test
	public void testCreateSession() {
		String id = (String) session.getId();
		Assert.assertTrue(Integer.valueOf(id).intValue() > 0);
	}

	@Test
	public void testRetrieveSession() {
		NomadSession storedSession = repository.findOne(session.getId());
		Assert.assertNotNull(storedSession);
	}

	@Test
	public void testExpireSession() throws InterruptedException, StoppedSessionException {
		try {
			session.setTimeout(100);
			LOG.info("set timeout to 100ms");
			Thread.sleep(150);
			LOG.info("getting session after timeout expiry");
			NomadSession storedSession = repository.findOne(session.getId());
			storedSession.validate();
		} catch (ExpiredSessionException e) {
			Assert.assertTrue(e.getMessage().contains("has expired"));
		}
	}

	@Test
	public void testCustomField() {
		String stateWander = "wandering";
		String stateLook = "looking";
		String id1 = setState(session, stateWander);
		NomadSession s2 = createSession();
		String id2 = setState(s2, stateLook);
		NomadSession s3 = createSession();
		String id3 = setState(s3, stateWander);
		List<NomadSession> wandering = repository.findByState(stateWander);
		List<NomadSession> looking = repository.findByState(stateLook);
		Assert.assertEquals(2, wandering.size());
		Assert.assertEquals(id1, wandering.get(0).getId());
		Assert.assertEquals(id3, wandering.get(1).getId());
		Assert.assertEquals(1, looking.size());
		Assert.assertEquals(id2, looking.get(0).getId());
	}

	private String setState(NomadSession s, String state) {
		// Because I have first-class NomadSession here, there's no need to look it up
		// in the repository
		s.setState(state);
		repository.update(s);
		String id1 = (String) s.getId();
		return id1;
	}

	private NomadSession createSession() {
		NomadSession s = sessionFactory.createSession(null);
		repository.create(s);
		Assert.assertNotNull(s);
		return s;
	}

}
