package com.yahoo.sorelmitra.shiro.nomad;

import java.io.Serializable;
import java.util.Collection;
import java.util.List;

import org.apache.shiro.session.Session;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;

@Component
@Profile("jdbc")
public class NomadJdbcRepository implements NomadRepository {

	private static Logger LOG = LoggerFactory.getLogger(NomadJdbcRepository.class);

	@Autowired
	private NomadJpaRepositoryInterface repository;

	@Override
	public void create(NomadSession s) {
		repository.save(s);
		LOG.info("Created session " + s);
	}

	@Override
	public Session read(Serializable id) {
		NomadSession s = repository.findOne(id);
		LOG.info("Read session for id " + id + ": " + s);
		return s;
	}

	@Override
	public void update(NomadSession s) {
		repository.save(s);
		LOG.info("Updated session " + s);
	}

	@Override
	public void delete(NomadSession s) {
		repository.delete(s);
		LOG.info("Deleted session " + s);
	}

	@Override
	public Collection<Session> getActiveSessions() {
		// TODO Implement
		return null;
	}

	@Override
	public List<NomadSession> findByState(String state) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public NomadSession findOne(Serializable id) {
		// TODO Auto-generated method stub
		return null;
	}

}
