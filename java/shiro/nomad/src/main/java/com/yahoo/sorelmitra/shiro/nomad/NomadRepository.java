package com.yahoo.sorelmitra.shiro.nomad;

import java.io.Serializable;
import java.util.Collection;

import org.apache.shiro.session.Session;

public interface NomadRepository {

	void create(NomadSession s);

	Session read(Serializable id);

	void update(NomadSession s);

	void delete(NomadSession s);

	Collection<Session> getActiveSessions();

}
