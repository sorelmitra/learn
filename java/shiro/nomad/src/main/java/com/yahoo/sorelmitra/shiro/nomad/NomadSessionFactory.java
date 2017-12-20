package com.yahoo.sorelmitra.shiro.nomad;

import org.apache.shiro.session.Session;
import org.apache.shiro.session.mgt.SessionContext;
import org.apache.shiro.session.mgt.SessionFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class NomadSessionFactory implements SessionFactory {

	private static Logger LOG = LoggerFactory.getLogger(NomadSessionFactory.class);

	public Session createSession(SessionContext initData) {
		if (initData != null) {
			String host = initData.getHost();
			if (host != null) {
				return new NomadSession(host);
			}
		}
		return new NomadSession();
	}
}
