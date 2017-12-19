package com.yahoo.sorelmitra.shiro.nomad;

import org.apache.shiro.mgt.DefaultSecurityManager;
import org.apache.shiro.mgt.SecurityManager;
import org.apache.shiro.session.mgt.DefaultSessionManager;
import org.apache.shiro.session.mgt.eis.EnterpriseCacheSessionDAO;
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
	public DefaultSessionManager sessionManager() {
		DefaultSessionManager sessionManager = new DefaultSessionManager();
		sessionManager.setSessionDAO(sessionDao());
		return sessionManager;
	}

	@Bean
	public EnterpriseCacheSessionDAO sessionDao() {
		EnterpriseCacheSessionDAO sessionDAO = new EnterpriseCacheSessionDAO();
		sessionDAO.setSessionIdGenerator(sessionIdGenerator());
		return sessionDAO;
	}

	@Bean
	public NomadSessionIdGenerator sessionIdGenerator() {
		return new NomadSessionIdGenerator();
	}

}
