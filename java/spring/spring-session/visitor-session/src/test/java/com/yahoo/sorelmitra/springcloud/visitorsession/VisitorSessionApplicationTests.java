package com.yahoo.sorelmitra.springcloud.visitorsession;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Types;
import java.util.Map;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.datasource.DataSourceTransactionManager;
import org.springframework.jdbc.support.rowset.SqlRowSet;
import org.springframework.jdbc.support.rowset.SqlRowSetMetaData;
import org.springframework.session.FindByIndexNameSessionRepository;
import org.springframework.session.Session;
import org.springframework.session.SessionRepository;
import org.springframework.session.jdbc.JdbcOperationsSessionRepository;
import org.springframework.session.jdbc.config.annotation.web.http.EnableJdbcHttpSession;
import org.springframework.test.context.junit4.SpringRunner;

import com.zaxxer.hikari.HikariDataSource;


@EnableJdbcHttpSession
@RunWith(SpringRunner.class)
@SpringBootTest
public class VisitorSessionApplicationTests {
	private static final Logger LOG = LoggerFactory.getLogger(VisitorSessionApplicationTests.class);
	
	private String sqlCheckTableEmptyState;
	private JdbcTemplate jdbcTemplate;
	
	@Autowired
	private HikariDataSource source;
	
	@Value("${spring.session.jdbc.table-name}")
	private String tableName;

	@Before
	public void setUp() {
		sqlCheckTableEmptyState = "SELECT * FROM visitor.SESSION";
		jdbcTemplate = new JdbcTemplate(source);
	}
	
	@After
	public void tearDown() throws SQLException {
	}
	
	@Test
	public void tablesCreated() throws SQLException {
		SqlRowSet rowSet = jdbcTemplate.queryForRowSet(sqlCheckTableEmptyState);
		SqlRowSetMetaData metaData = rowSet.getMetaData();
		Assert.assertEquals(5, metaData.getColumnCount());
		Assert.assertEquals(Types.CHAR, metaData.getColumnType(1));
		Assert.assertEquals(Types.BIGINT, metaData.getColumnType(2));
		Assert.assertEquals(Types.BIGINT, metaData.getColumnType(3));
		Assert.assertEquals(Types.INTEGER, metaData.getColumnType(4));
		Assert.assertEquals(Types.VARCHAR, metaData.getColumnType(5));
	}
	
	@Test
	public void sessionCreated() {
		MyJdbcOperationsSessionRepository jdbcOperationsSessionRepository =
			new MyJdbcOperationsSessionRepository(
				source, new DataSourceTransactionManager(source));
		jdbcOperationsSessionRepository.setTableName(tableName);
		// Notice the ugly warning
		SessionRepository repo = jdbcOperationsSessionRepository;

		// Can't do this, VisitorSession and JdbcSession are on separate hierachies from Session:
		// VisitorSession toSave = (VisitorSession) repo.createSession();
		Session toSave = repo.createSession();
		toSave.setAttribute("id", "12");
		toSave.setAttribute("state", "INCOMING");
		toSave.setAttribute("last_msg_timestamp", "2017-12-14 15:22:00");

		// Notice the ugly warning
		repo.save(toSave);
		
		// Can't do this, VisitorSession and JdbcSession are on separate hierachies from Session:
		// VisitorSession session = (VisitorSession) repo.getSession(toSave.getId());
		Session session = repo.getSession(toSave.getId());
		
		Assert.assertEquals("12", session.getAttribute("id"));
		Assert.assertEquals("INCOMING", session.getAttribute("state"));
		Assert.assertEquals("2017-12-14 15:22:00", session.getAttribute("last_msg_timestamp"));
		
		// Can't do this, JdbcSession is not visible: Map<String, JdbcSession> touristSessions = jdbcOperationsSessionRepository.findByIndexNameAndIndexValue(FindByIndexNameSessionRepository.PRINCIPAL_NAME_INDEX_NAME, "tourist");
		Map<String, Session> touristSessions = jdbcOperationsSessionRepository.getAllSessionsWithPrincipalName("tourist");
		Assert.assertEquals(null, touristSessions); // Can't access JdbcSession, not even in the extended class
	}

	public HikariDataSource getSource() {
		return source;
	}

	public void setSource(HikariDataSource source) {
		this.source = source;
	}

}
