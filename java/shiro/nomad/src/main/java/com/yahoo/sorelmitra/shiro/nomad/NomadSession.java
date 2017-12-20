package com.yahoo.sorelmitra.shiro.nomad;

import java.io.Serializable;
import java.util.Date;

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
	private long timeout;

	public long getTimeout() {
		return timeout;
	}

	public void setTimeout(long timeout) {
		this.timeout = timeout;
	}

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

	@Override
	public String toString() {
		return "id: " + getId() + ", started: " + millis(getStartTimestamp()) + ", last access: "
				+ millis(getLastAccessTime()) + ", stopped: " + millis(getStopTimestamp());
	}

	private long millis(Date timestamp) {
		if (timestamp == null) {
			return 0;
		}
		return timestamp.toInstant().toEpochMilli();
	}

	@Override
	public void setLastAccessTime(Date lastAccessTime) {
		LOG.info("set last access time to " + millis(lastAccessTime) + ": " + this);
		super.setLastAccessTime(lastAccessTime);
	}

	@Override
	public void touch() {
		LOG.info("touched: " + this);
		super.touch();
	}
}
