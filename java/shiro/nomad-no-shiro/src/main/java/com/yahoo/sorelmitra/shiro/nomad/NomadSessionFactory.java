package com.yahoo.sorelmitra.shiro.nomad;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class NomadSessionFactory {

	@Autowired
	private NomadSessionIdGenerator sessionIdGenerator;

	private static Logger LOG = LoggerFactory.getLogger(NomadSessionFactory.class);

	public NomadSession createSession(SessionContext initData) {
		NomadSession s = null;
		if (initData != null) {
			String host = initData.getHost();
			if (host != null) {
				s = new NomadSession(host);
			}
		}
		if (s == null) {
			s = new NomadSession();
		}
		s.setId(sessionIdGenerator.generateId(s));
		return s;
	}
}
