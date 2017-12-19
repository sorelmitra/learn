package com.yahoo.sorelmitra.shiro.nomad;

import java.io.Serializable;

import org.apache.shiro.session.Session;
import org.apache.shiro.session.mgt.eis.SessionIdGenerator;

public class NomadSessionIdGenerator implements SessionIdGenerator {

	private static int id = 0;
	
	@Override
	public Serializable generateId(Session arg0) {
		id++;
		return Integer.toString(id);
	}

}
