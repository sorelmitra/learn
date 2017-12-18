package org.springframework.session.jdbc;

import java.util.HashMap;
import java.util.Map;

import javax.sql.DataSource;

import org.springframework.session.Session;
import org.springframework.transaction.PlatformTransactionManager;

// Hey, I'm a friend of JdbcOperationsSessionRepository, because I lay in the same package
public class FriendOfJdbcSessionRepo extends JdbcOperationsSessionRepository {

	public FriendOfJdbcSessionRepo(DataSource dataSource, PlatformTransactionManager platformTransactionManager) {
		super(dataSource, platformTransactionManager);
	}

	public Map<String, Session> getAllSessionsWithPrincipalName(String principalName) {
		Map<String, JdbcSession> principalNameSessions = findByIndexNameAndIndexValue(PRINCIPAL_NAME_INDEX_NAME, principalName);
		Map<String, Session> result = new HashMap<String, Session>();
		for (String sessionId : principalNameSessions.keySet()) {
			result.put(sessionId, (Session) principalNameSessions.get(sessionId));
		}
		return result;
	}
}

