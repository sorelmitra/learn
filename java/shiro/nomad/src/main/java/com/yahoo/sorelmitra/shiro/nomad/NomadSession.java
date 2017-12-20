package com.yahoo.sorelmitra.shiro.nomad;

import org.apache.shiro.session.mgt.SimpleSession;

public class NomadSession extends SimpleSession {

	// Serialization reminder:
	// You _MUST_ change this number if you introduce a change to this class
	// that is NOT serialization backwards compatible. Serialization-compatible
	// changes do not require a change to this number. If you need to generate
	// a new number in this case, use the JDK's 'serialver' program to generate it.
	private static final long serialVersionUID = -5446279961699030633L;

	public NomadSession() {
		super();
	}

	public NomadSession(String host) {
		super(host);
	}
}
