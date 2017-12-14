package com.yahoo.sorelmitra.springcloud.visitorsession;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Types;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.jdbc.datasource.DataSourceTransactionManager;
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
	private PreparedStatement ps = null;
	
	@Autowired
	private HikariDataSource source;
	
	@Autowired
	private VisitorRepository visitorRepository;

	@Value("${spring.session.jdbc.table-name}")
	private String tableName;

	@Before
	public void setUp() {
		sqlCheckTableEmptyState = "SELECT * FROM visitor.state";
	}
	
	@After
	public void tearDown() throws SQLException {
		closePs();
	}
	
	@Test
	public void tablesCreated() throws SQLException {
		ResultSet rs = runStatement(sqlCheckTableEmptyState, true);
		ResultSetMetaData metaData = rs.getMetaData();
		Assert.assertEquals(3, metaData.getColumnCount());
		Assert.assertEquals(Types.CHAR, metaData.getColumnType(1));
		Assert.assertEquals(Types.CHAR, metaData.getColumnType(2));
		Assert.assertEquals(Types.DATE, metaData.getColumnType(3));
	}
	
	@Ignore
	@Test
	public void sessionCreated() {
		JdbcOperationsSessionRepository jdbcOperationsSessionRepository = new JdbcOperationsSessionRepository(source, new DataSourceTransactionManager(source));
		jdbcOperationsSessionRepository.setTableName(tableName);
		SessionRepository<? extends Session> repository = jdbcOperationsSessionRepository;
		visitorRepository.setRepository(repository);
		String id = visitorRepository.saveSession();
		Session session = visitorRepository.getRepository().getSession(id);
		Assert.assertEquals("12", session.getAttribute("id"));
	}

	private ResultSet runStatement(String sqlString, boolean isQuery) throws SQLException {
		ResultSet rs = null;
		try {
			LOG.info("Connected to " + source.getJdbcUrl() + "; running SQL " + sqlString);
			closePs();
			ps = source.getConnection().prepareStatement(sqlString);
			if (isQuery) {
				rs = ps.executeQuery();
			} else {
				ps.executeUpdate();
			}
			LOG.info("SQL " + sqlString + " executed successfully");
		} finally {
			source.close();
		}

		return rs;
	}

	private void closePs() throws SQLException {
		if (ps != null) {
			ps.close();
		}
	}

	public HikariDataSource getSource() {
		return source;
	}

	public void setSource(HikariDataSource source) {
		this.source = source;
	}

}
