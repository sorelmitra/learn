package com.yahoo.sorelmitra.shiro.nomad;

import java.io.Serializable;
import java.util.Collection;

import org.apache.shiro.session.Session;
import org.apache.shiro.session.UnknownSessionException;
import org.apache.shiro.session.mgt.eis.AbstractSessionDAO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

public class NomadSessionDAO extends AbstractSessionDAO {

	private static Logger LOG = LoggerFactory.getLogger(NomadSessionDAO.class);

	@Autowired
	private NomadRepository repository;

	@Override
	public Collection<Session> getActiveSessions() {
		return repository.getActiveSessions();
	}

	@Override
	protected Serializable doCreate(Session arg0) {
		NomadSession s = (NomadSession) arg0;
		Serializable id = generateSessionId(s);
		s.setId(id);
		repository.create(s);
		return id;
	}

	@Override
	protected Session doReadSession(Serializable id) {
		Session s = repository.read(id);
		return s;
	}

	@Override
	public void update(Session arg0) throws UnknownSessionException {
		NomadSession s = (NomadSession) arg0;
		repository.update(s);
	}

	@Override
	public void delete(Session arg0) {
		NomadSession s = (NomadSession) arg0;
		repository.delete(s);
	}
}
