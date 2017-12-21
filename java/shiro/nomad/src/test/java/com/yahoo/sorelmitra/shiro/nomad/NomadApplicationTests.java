package com.yahoo.sorelmitra.shiro.nomad;

import java.util.List;

import org.apache.shiro.SecurityUtils;
import org.apache.shiro.mgt.SecurityManager;
import org.apache.shiro.session.ExpiredSessionException;
import org.apache.shiro.session.Session;
import org.apache.shiro.session.StoppedSessionException;
import org.apache.shiro.session.UnknownSessionException;
import org.apache.shiro.session.mgt.DefaultSessionKey;
import org.apache.shiro.session.mgt.SessionManager;
import org.apache.shiro.subject.Subject;
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
	private SecurityManager securityManager;

	@Autowired
	private SessionManager sessionManager;

	@Autowired
	private NomadRepository repository;

	private Session session;
	private Subject currentUser;

	@Before
	public void setUp() {
		SecurityUtils.setSecurityManager(securityManager);
		currentUser = SecurityUtils.getSubject();
		session = createSession();
	}

	@After
	public void tearDown() {
		try {
			currentUser.logout();
		} catch (UnknownSessionException | StoppedSessionException e) {
		}
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
		Assert.assertEquals(id1, wandering.get(0));
		Assert.assertEquals(id3, wandering.get(1));
		Assert.assertEquals(1, looking.size());
		Assert.assertEquals(id2, wandering.get(0));
	}

	private String setState(Session arg0, String state) {
		NomadSession s = (NomadSession) arg0;
		s.setState(state);
		String id1 = (String) s.getId();
		return id1;
	}

	private Session createSession() {
		Assert.assertNull(currentUser.getSession(false));
		Session s = currentUser.getSession();
		Assert.assertNotNull(s);
		return s;
	}

}
