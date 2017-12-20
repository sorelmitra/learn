package com.yahoo.sorelmitra.shiro.nomad;

import org.apache.shiro.mgt.DefaultSecurityManager;
import org.apache.shiro.mgt.SecurityManager;
import org.apache.shiro.session.SessionException;
import org.apache.shiro.session.mgt.DefaultSessionManager;
import org.apache.shiro.session.mgt.SessionManager;
import org.apache.shiro.session.mgt.eis.SessionDAO;
import org.apache.shiro.session.mgt.eis.SessionIdGenerator;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class ShiroConfiguration {

	public static final String DAO_IMPL_TYPE_MAP = "map";

	@Value("${shiro.config.session.daoimpl}")
	private String sessionDaoImplType;

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
		SessionDAO sessionDao;

		if (sessionDaoImplType.equalsIgnoreCase(DAO_IMPL_TYPE_MAP)) {
			NomadMapSessionDAO mapSessionDao = new NomadMapSessionDAO();
			mapSessionDao.setSessionIdGenerator(sessionIdGenerator());
			sessionDao = mapSessionDao;
		} else {
			throw new SessionException("Unknown Session DAO implementation type <<" + sessionDaoImplType
					+ ">> in application properties!");
		}

		return sessionDao;
	}

	@Bean
	public SessionIdGenerator sessionIdGenerator() {
		return new NomadSessionIdGenerator();
	}

	public String getSessionDaoImplType() {
		return sessionDaoImplType;
	}

	public void setSessionDaoImplType(String sessionDaoImplType) {
		this.sessionDaoImplType = sessionDaoImplType;
	}

}
