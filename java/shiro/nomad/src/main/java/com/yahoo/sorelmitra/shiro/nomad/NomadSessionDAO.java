package com.yahoo.sorelmitra.shiro.nomad;

import java.io.Serializable;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;

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
		Map<Serializable, Session> sessions = new HashMap<Serializable, Session>(); // dummy
		LinkedList<Session> activeSessions = new LinkedList<Session>();
		for (Serializable id : sessions.keySet()) {
			NomadSession s = (NomadSession) sessions.get(id);
			if (!s.isValid()) {
				continue;
			}
			if (s.isExpired()) {
				continue;
			}
			activeSessions.add(s);
		}
		return activeSessions;
	}

	@Override
	protected Serializable doCreate(Session arg0) {
		NomadSession s = (NomadSession) arg0;
		Serializable id = generateSessionId(s);
		s.setId(id);
		repository.create(s);
		LOG.info("Created session " + id + ": " + s);
		return id;
	}

	@Override
	protected Session doReadSession(Serializable id) {
		Session s = repository.read(id);
		LOG.info("Read session " + id + ": " + s);
		return s;
	}

	@Override
	public void update(Session arg0) throws UnknownSessionException {
		NomadSession s = (NomadSession) arg0;
		repository.update(s);
		LOG.info("Updated session " + s.getId() + ": " + s);
	}

	@Override
	public void delete(Session arg0) {
		NomadSession s = (NomadSession) arg0;
		repository.delete(s);
		LOG.info("Deleted session: " + s);
	}
}
