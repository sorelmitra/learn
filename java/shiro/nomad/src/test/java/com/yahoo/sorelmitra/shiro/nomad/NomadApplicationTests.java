package com.yahoo.sorelmitra.shiro.nomad;

import org.apache.shiro.SecurityUtils;
import org.apache.shiro.session.ExpiredSessionException;
import org.apache.shiro.session.Session;
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
	private SessionManager sessionManager;

	private Session session;
	private Subject currentUser;

	@Before
	public void setUp() {
		currentUser = SecurityUtils.getSubject();
		session = createSession();
	}

	@After
	public void tearDown() {
		try {
			currentUser.logout();
		} catch (UnknownSessionException e) {
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

	private Session createSession() {
		Assert.assertNull(currentUser.getSession(false));
		Session session = currentUser.getSession();
		Assert.assertNotNull(session);
		return session;
	}

}
