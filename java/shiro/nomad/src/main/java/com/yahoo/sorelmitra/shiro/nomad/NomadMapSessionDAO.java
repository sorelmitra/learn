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

public class NomadMapSessionDAO extends AbstractSessionDAO {

	private static Logger LOG = LoggerFactory.getLogger(NomadMapSessionDAO.class);

	private Map<Serializable, Session> sessions;

	public NomadMapSessionDAO() {
		sessions = new HashMap<Serializable, Session>();
	}

	@Override
	public Collection<Session> getActiveSessions() {
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
		sessions.put(id, s);
		LOG.info("Created session " + id + ": " + s);
		return id;
	}

	@Override
	protected Session doReadSession(Serializable id) {
		Session s = sessions.get(id);
		LOG.info("Read session " + id + ": " + s);
		return s;
	}

	@Override
	public void update(Session arg0) throws UnknownSessionException {
		NomadSession s = (NomadSession) arg0;
		sessions.put(s.getId(), s);
		LOG.info("Updated session " + s.getId() + ": " + s);
	}

	@Override
	public void delete(Session arg0) {
		NomadSession s = (NomadSession) arg0;
		sessions.remove(s.getId());
		LOG.info("Deleted session: " + s);
	}

}
