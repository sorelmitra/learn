package com.yahoo.sorelmitra.shiro.nomad;

import java.io.Serializable;

import javax.persistence.Entity;
import javax.persistence.Id;

import org.apache.shiro.session.mgt.SimpleSession;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Entity
public class NomadSession extends SimpleSession {

	private static Logger LOG = LoggerFactory.getLogger(NomadSession.class);

	// Serialization reminder:
	// You _MUST_ change this number if you introduce a change to this class
	// that is NOT serialization backwards compatible. Serialization-compatible
	// changes do not require a change to this number. If you need to generate
	// a new number in this case, use the JDK's 'serialver' program to generate it.
	private static final long serialVersionUID = -5446279961699030633L;

	@Id
	private Serializable id;

	public NomadSession() {
		super();
	}

	public NomadSession(String host) {
		super(host);
	}

	public Serializable getId() {
		return this.id;
	}

	public void setId(Serializable id) {
		this.id = id;
	}

}
