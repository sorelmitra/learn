package com.yahoo.sorelmitra.shiro.nomad;

import java.io.Serializable;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.apache.shiro.session.Session;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;

@Component
@Profile("map")
public class NomadMapRepository implements NomadRepository {

	private static Logger LOG = LoggerFactory.getLogger(NomadMapRepository.class);

	private Map<Serializable, NomadSession> sessions;

	public NomadMapRepository() {
		sessions = new HashMap<Serializable, NomadSession>();
	}

	@Override
	public void create(NomadSession s) {
		sessions.put(s.getId(), s);
		LOG.info("Created session " + s);
	}

	@Override
	public Session read(Serializable id) {
		NomadSession s = sessions.get(id);
		LOG.info("Read session for id " + id + ": " + s);
		return s;
	}

	@Override
	public void update(NomadSession s) {
		sessions.put(s.getId(), s);
		LOG.info("Updated session " + s);
	}

	@Override
	public void delete(NomadSession s) {
		sessions.remove(s);
		LOG.info("Deleted session " + s);
	}

	@Override
	public Collection<Session> getActiveSessions() {
		LinkedList<Session> activeSessions = new LinkedList<Session>();
		walkValidSessions(new MapWalker() {
			@Override
			public void process(NomadSession s) {
				activeSessions.add(s);
			}
		});
		return activeSessions;
	}

	private interface MapWalker {
		void process(NomadSession s);
	}

	private void walkValidSessions(MapWalker mapWalker) {
		for (Serializable id : sessions.keySet()) {
			NomadSession s = (NomadSession) sessions.get(id);
			if (!s.isValid()) {
				continue;
			}
			if (s.isExpired()) {
				continue;
			}
			mapWalker.process(s);
		}
	}

	@Override
	public List<NomadSession> findByState(String state) {
		LinkedList<NomadSession> foundSessions = new LinkedList<NomadSession>();
		walkValidSessions(new MapWalker() {
			@Override
			public void process(NomadSession s) {
				if (state.equalsIgnoreCase(s.getState())) {
					foundSessions.add(s);
				}
			}
		});
		return foundSessions;
	}

}
