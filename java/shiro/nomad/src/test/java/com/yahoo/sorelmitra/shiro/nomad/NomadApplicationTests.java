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
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

@RunWith(SpringRunner.class)
@SpringBootTest
public class NomadApplicationTests {

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
			Thread.sleep(150);
			Session storedSession = sessionManager.getSession(new DefaultSessionKey(session.getId()));
			Assert.assertNotNull(storedSession);
			Assert.fail();
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
