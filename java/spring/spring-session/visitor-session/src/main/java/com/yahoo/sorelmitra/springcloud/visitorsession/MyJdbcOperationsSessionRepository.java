package com.yahoo.sorelmitra.springcloud.visitorsession;

import java.util.Map;

import javax.sql.DataSource;

import org.springframework.session.Session;
import org.springframework.session.jdbc.JdbcOperationsSessionRepository;
import org.springframework.transaction.PlatformTransactionManager;

public class MyJdbcOperationsSessionRepository extends JdbcOperationsSessionRepository {

	public MyJdbcOperationsSessionRepository(DataSource dataSource, PlatformTransactionManager platformTransactionManager) {
		super(dataSource, platformTransactionManager);
	}

	public Map<String, Session> getAllSessionsWithPrincipalName(String principalName) {
		// Can't do this, JdbcSession is not visible: Map<String, JdbcSession> principalNameSessions = findByIndexNameAndIndexValue(PRINCIPAL_NAME_INDEX_NAME, principalName);
		return null;
	}
}
