package com.yahoo.sorelmitra.shiro.nomad;

import org.apache.shiro.mgt.DefaultSecurityManager;
import org.apache.shiro.mgt.SecurityManager;
import org.apache.shiro.session.mgt.DefaultSessionManager;
import org.apache.shiro.session.mgt.SessionFactory;
import org.apache.shiro.session.mgt.SessionManager;
import org.apache.shiro.session.mgt.eis.SessionDAO;
import org.apache.shiro.session.mgt.eis.SessionIdGenerator;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class ShiroConfiguration {

	@Bean
	public SecurityManager securityManager() {
		DefaultSecurityManager securityManager = new DefaultSecurityManager();
		securityManager.setSessionManager(sessionManager());
		return securityManager;
	}

	@Bean
	public SessionManager sessionManager() {
		DefaultSessionManager sessionManager = new DefaultSessionManager();
		sessionManager.setSessionFactory(sessionFactory());
		sessionManager.setSessionDAO(sessionDao());
		return sessionManager;
	}

	@Bean
	public SessionFactory sessionFactory() {
		NomadSessionFactory sessionFactory = new NomadSessionFactory();
		return sessionFactory;
	}

	@Bean
	public SessionDAO sessionDao() {
		NomadSessionDAO sessionDao = new NomadSessionDAO();
		sessionDao.setSessionIdGenerator(sessionIdGenerator());
		return sessionDao;
	}

	@Bean
	public SessionIdGenerator sessionIdGenerator() {
		return new NomadSessionIdGenerator();
	}

}
