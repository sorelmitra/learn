package com.yahoo.sorelmitra.shiro.nomad;

import org.apache.shiro.SecurityUtils;
import org.apache.shiro.session.Session;
import org.apache.shiro.subject.Subject;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

@RunWith(SpringRunner.class)
@SpringBootTest
public class NomadApplicationTests {

	@Test
	public void createSession() {
		Subject currentUser = SecurityUtils.getSubject();
		Assert.assertNull(currentUser.getSession(false));
		Session session = currentUser.getSession();
		Assert.assertNotNull(session);
	}

}
