package com.yahoo.sorelmitra.shiro.nomad;

import java.util.List;

import org.apache.shiro.session.ExpiredSessionException;
import org.apache.shiro.session.Session;
import org.apache.shiro.session.mgt.DefaultSessionContext;
import org.apache.shiro.session.mgt.DefaultSessionKey;
import org.apache.shiro.session.mgt.SessionContext;
import org.apache.shiro.session.mgt.SessionManager;
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
	private SessionManager sessionManager;

	@Autowired
	private NomadRepository repository;

	private Session session;

	private SessionContext context;

	@Before
	public void setUp() {
		context = new DefaultSessionContext();
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
		Session storedSession = sessionManager.getSession(new DefaultSessionKey(session.getId()));
		Assert.assertNotNull(storedSession);
	}

	@Test
	public void testExpireSession() throws InterruptedException {
		try {
			session.setTimeout(100);
			LOG.info("set timeout to 100ms");
			Thread.sleep(150);
			LOG.info("getting session after timeout expiry");
			Session storedSession = sessionManager.getSession(new DefaultSessionKey(session.getId()));
			Assert.assertNull(storedSession);
		} catch (ExpiredSessionException e) {
			Assert.assertTrue(e.getMessage().contains("has expired"));
		}
	}

	@Test
	public void testCustomField() {
		String stateWander = "wandering";
		String stateLook = "looking";
		String id1 = setState(session, stateWander);
		Session s2 = createSession();
		String id2 = setState(s2, stateLook);
		Session s3 = createSession();
		String id3 = setState(s3, stateWander);
		List<NomadSession> wandering = repository.findByState(stateWander);
		List<NomadSession> looking = repository.findByState(stateLook);
		Assert.assertEquals(2, wandering.size());
		Assert.assertEquals(id1, wandering.get(0).getId());
		Assert.assertEquals(id3, wandering.get(1).getId());
		Assert.assertEquals(1, looking.size());
		Assert.assertEquals(id2, looking.get(0).getId());
	}

	private String setState(Session arg0, String state) {
		NomadSession s = repository.findOne(arg0.getId());
		s.setState(state);
		String id1 = (String) s.getId();
		return id1;
	}

	private Session createSession() {
		Session s = sessionManager.start(context);
		Assert.assertNotNull(s);
		return s;
	}

}
