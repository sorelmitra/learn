package com.yahoo.sorelmitra.shiro.nomad;

import org.apache.shiro.mgt.DefaultSecurityManager;
import org.apache.shiro.mgt.SecurityManager;
import org.apache.shiro.session.mgt.DefaultSessionManager;
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
		sessionManager.setSessionDAO(sessionDao());
		return sessionManager;
	}

	@Bean
	public SessionDAO sessionDao() {
		NomadMapSessionDAO sessionDAO = new NomadMapSessionDAO();
		sessionDAO.setSessionIdGenerator(sessionIdGenerator());
		return sessionDAO;
	}

	@Bean
	public SessionIdGenerator sessionIdGenerator() {
		return new NomadSessionIdGenerator();
	}

}
