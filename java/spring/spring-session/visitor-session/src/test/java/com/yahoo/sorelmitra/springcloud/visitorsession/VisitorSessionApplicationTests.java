package com.yahoo.sorelmitra.springcloud.visitorsession;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Types;

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
import org.springframework.test.context.junit4.SpringRunner;

import com.zaxxer.hikari.HikariDataSource;


@RunWith(SpringRunner.class)
@SpringBootTest
public class VisitorSessionApplicationTests {
	private static final Logger LOG = LoggerFactory.getLogger(VisitorSessionApplicationTests.class);
	
	private String sqlCheckTableEmptyState;
	private PreparedStatement ps = null;
	
	@Autowired
	private HikariDataSource source;

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

	private HikariDataSource createHikariDataSourceInstance(String dataSourceUrl, String driver) {
		HikariDataSource hikariDataSource = new HikariDataSource();
		hikariDataSource.setJdbcUrl(dataSourceUrl);
		hikariDataSource.setDriverClassName(driver);
		return hikariDataSource;
	}

	public HikariDataSource getSource() {
		return source;
	}

	public void setSource(HikariDataSource source) {
		this.source = source;
	}

}
