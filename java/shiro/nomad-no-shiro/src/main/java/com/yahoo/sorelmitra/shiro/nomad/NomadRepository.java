package com.yahoo.sorelmitra.shiro.nomad;

import java.io.Serializable;
import java.util.Collection;
import java.util.List;

public interface NomadRepository {

	void create(NomadSession s);

	NomadSession read(Serializable id);

	void update(NomadSession s);

	void delete(NomadSession s);

	Collection<NomadSession> getActiveSessions();

	List<NomadSession> findByState(String state);

	NomadSession findOne(Serializable id);

}
