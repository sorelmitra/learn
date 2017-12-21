package com.yahoo.sorelmitra.shiro.nomad;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.text.DateFormat;
import java.util.Collection;
import java.util.Date;

import javax.persistence.Entity;
import javax.persistence.Id;

import org.apache.shiro.session.ExpiredSessionException;
import org.apache.shiro.session.InvalidSessionException;
import org.apache.shiro.session.StoppedSessionException;
import org.apache.shiro.session.mgt.DefaultSessionManager;
import org.apache.shiro.session.mgt.ValidatingSession;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Session implementation insipred from
 * {@link org.apache.shiro.session.SimpleSession} Adapted to be used with JPA.
 *
 */
@Entity
public class NomadSession implements ValidatingSession, Serializable {

	// Serialization reminder:
	// You _MUST_ change this number if you introduce a change to this class
	// that is NOT serialization backwards compatible.
	private static final long serialVersionUID = -5446279961699030633L;

	private transient static final Logger log = LoggerFactory.getLogger(NomadSession.class);

	protected static final long MILLIS_PER_SECOND = 1000;
	protected static final long MILLIS_PER_MINUTE = 60 * MILLIS_PER_SECOND;
	protected static final long MILLIS_PER_HOUR = 60 * MILLIS_PER_MINUTE;

	// serialization bitmask fields. DO NOT CHANGE THE ORDER THEY ARE DECLARED!
	static int bitIndexCounter = 0;
	private static final int ID_BIT_MASK = 1 << bitIndexCounter++;
	private static final int START_TIMESTAMP_BIT_MASK = 1 << bitIndexCounter++;
	private static final int STOP_TIMESTAMP_BIT_MASK = 1 << bitIndexCounter++;
	private static final int LAST_ACCESS_TIME_BIT_MASK = 1 << bitIndexCounter++;
	private static final int TIMEOUT_BIT_MASK = 1 << bitIndexCounter++;
	private static final int EXPIRED_BIT_MASK = 1 << bitIndexCounter++;
	private static final int HOST_BIT_MASK = 1 << bitIndexCounter++;

	@Id
	private Serializable id;
	private Date startTimestamp;
	private Date stopTimestamp;
	private Date lastAccessTime;
	private long timeout;
	private boolean expired;
	private String host;

	public NomadSession() {
		this.timeout = DefaultSessionManager.DEFAULT_GLOBAL_SESSION_TIMEOUT; // TODO - remove concrete reference to
																				// DefaultSessionManager
		this.startTimestamp = new Date();
		this.lastAccessTime = this.startTimestamp;
	}

	public NomadSession(String host) {
		this();
		this.host = host;
	}

	public Serializable getId() {
		return this.id;
	}

	public void setId(Serializable id) {
		this.id = id;
	}

	public Date getStartTimestamp() {
		return startTimestamp;
	}

	public void setStartTimestamp(Date startTimestamp) {
		this.startTimestamp = startTimestamp;
	}

	/**
	 * Returns the time the session was stopped, or <tt>null</tt> if the session is
	 * still active.
	 * 
	 * See {@link org.apache.shiro.session.SimpleSession} for more details.
	 */
	public Date getStopTimestamp() {
		return stopTimestamp;
	}

	public void setStopTimestamp(Date stopTimestamp) {
		this.stopTimestamp = stopTimestamp;
	}

	public Date getLastAccessTime() {
		return lastAccessTime;
	}

	public void setLastAccessTime(Date lastAccessTime) {
		this.lastAccessTime = lastAccessTime;
	}

	/**
	 * Returns true if this session has expired, false otherwise. If the session has
	 * expired, no further user interaction with the system may be done under this
	 * session.
	 */
	public boolean isExpired() {
		return expired;
	}

	public void setExpired(boolean expired) {
		this.expired = expired;
	}

	public long getTimeout() {
		return timeout;
	}

	public void setTimeout(long timeout) {
		this.timeout = timeout;
	}

	public String getHost() {
		return host;
	}

	public void setHost(String host) {
		this.host = host;
	}

	public void touch() {
		this.lastAccessTime = new Date();
	}

	public void stop() {
		if (this.stopTimestamp == null) {
			this.stopTimestamp = new Date();
		}
	}

	protected boolean isStopped() {
		return getStopTimestamp() != null;
	}

	protected void expire() {
		stop();
		this.expired = true;
	}

	/**
	 */
	public boolean isValid() {
		return !isStopped() && !isExpired();
	}

	/**
	 * Determines if this session is expired.
	 *
	 * @return true if the specified session has expired, false otherwise.
	 */
	protected boolean isTimedOut() {

		if (isExpired()) {
			return true;
		}

		long timeout = getTimeout();

		if (timeout >= 0l) {

			Date lastAccessTime = getLastAccessTime();

			if (lastAccessTime == null) {
				String msg = "session.lastAccessTime for session with id [" + getId()
						+ "] is null.  This value must be set at "
						+ "least once, preferably at least upon instantiation.  Please check the "
						+ getClass().getName() + " implementation and ensure "
						+ "this value will be set (perhaps in the constructor?)";
				throw new IllegalStateException(msg);
			}

			long expireTimeMillis = System.currentTimeMillis() - timeout;
			Date expireTime = new Date(expireTimeMillis);
			return lastAccessTime.before(expireTime);
		} else {
			if (log.isTraceEnabled()) {
				log.trace("No timeout for session with id [" + getId() + "].  Session is not considered expired.");
			}
		}

		return false;
	}

	public void validate() throws InvalidSessionException {
		// check for stopped:
		if (isStopped()) {
			// timestamp is set, so the session is considered stopped:
			String msg = "Session with id [" + getId() + "] has been "
					+ "explicitly stopped.  No further interaction under this session is " + "allowed.";
			throw new StoppedSessionException(msg);
		}

		// check for expiration
		if (isTimedOut()) {
			expire();

			// throw an exception explaining details of why it expired:
			Date lastAccessTime = getLastAccessTime();
			long timeout = getTimeout();

			Serializable sessionId = getId();

			DateFormat df = DateFormat.getInstance();
			String msg = "Session with id [" + sessionId + "] has expired. " + "Last access time: "
					+ df.format(lastAccessTime) + ".  Current time: " + df.format(new Date())
					+ ".  Session timeout is set to " + timeout / MILLIS_PER_SECOND + " seconds ("
					+ timeout / MILLIS_PER_MINUTE + " minutes)";
			if (log.isTraceEnabled()) {
				log.trace(msg);
			}
			throw new ExpiredSessionException(msg);
		}
	}

	/**
	 * Returns {@code true} if the specified argument is an {@code instanceof}
	 * {@code NomadSession} and both {@link #getId() id}s are equal. If the argument
	 * is a {@code NomadSession} and either 'this' or the argument does not yet have
	 * an ID assigned, the value of {@link #onEquals(NomadSession) onEquals} is
	 * returned, which does a necessary attribute-based comparison when IDs are not
	 * available.
	 * 
	 * See {@link org.apache.shiro.session.SimpleSession} for more details.
	 *
	 * @param obj
	 *            the object to compare with this one for equality.
	 * @return {@code true} if this object is equivalent to the specified argument,
	 *         {@code false} otherwise.
	 */
	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj instanceof NomadSession) {
			NomadSession other = (NomadSession) obj;
			Serializable thisId = getId();
			Serializable otherId = other.getId();
			if (thisId != null && otherId != null) {
				return thisId.equals(otherId);
			} else {
				// fall back to an attribute based comparison:
				return onEquals(other);
			}
		}
		return false;
	}

	/**
	 * Provides an attribute-based comparison (no ID comparison) - incurred
	 * <em>only</em> when 'this' or the session object being compared for equality
	 * do not have a session id.
	 *
	 * @param ss
	 *            the NomadSession instance to compare for equality.
	 * @return true if all the attributes, except the id, are equal to this object's
	 *         attributes.
	 */
	protected boolean onEquals(NomadSession ss) {
		return (getStartTimestamp() != null ? getStartTimestamp().equals(ss.getStartTimestamp())
				: ss.getStartTimestamp() == null)
				&& (getStopTimestamp() != null ? getStopTimestamp().equals(ss.getStopTimestamp())
						: ss.getStopTimestamp() == null)
				&& (getLastAccessTime() != null ? getLastAccessTime().equals(ss.getLastAccessTime())
						: ss.getLastAccessTime() == null)
				&& (getTimeout() == ss.getTimeout()) && (isExpired() == ss.isExpired())
				&& (getHost() != null ? getHost().equals(ss.getHost()) : ss.getHost() == null);
	}

	/**
	 * Returns the hashCode. If the {@link #getId() id} is not {@code null}, its
	 * hashcode is returned immediately. If it is {@code null}, an attributes-based
	 * hashCode will be calculated and returned.
	 * 
	 * See {@link org.apache.shiro.session.SimpleSession} for more details.
	 *
	 * @return this object's hashCode
	 */
	@Override
	public int hashCode() {
		Serializable id = getId();
		if (id != null) {
			return id.hashCode();
		}
		int hashCode = getStartTimestamp() != null ? getStartTimestamp().hashCode() : 0;
		hashCode = 31 * hashCode + (getStopTimestamp() != null ? getStopTimestamp().hashCode() : 0);
		hashCode = 31 * hashCode + (getLastAccessTime() != null ? getLastAccessTime().hashCode() : 0);
		hashCode = 31 * hashCode + Long.valueOf(Math.max(getTimeout(), 0)).hashCode();
		hashCode = 31 * hashCode + Boolean.valueOf(isExpired()).hashCode();
		hashCode = 31 * hashCode + (getHost() != null ? getHost().hashCode() : 0);
		return hashCode;
	}

	/**
	 * Returns the string representation of this NomadSession, equal to
	 * <code>getClass().getName() + &quot;,id=&quot; + getId()</code>.
	 *
	 * @return the string representation of this NomadSession, equal to
	 *         <code>getClass().getName() + &quot;,id=&quot; + getId()</code>.
	 */
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append(getClass().getName()).append(",id=").append(getId());
		return sb.toString();
	}

	/**
	 * Serializes this object to the specified output stream for JDK Serialization.
	 *
	 * @param out
	 *            output stream used for Object serialization.
	 * @throws IOException
	 *             if any of this object's fields cannot be written to the stream.
	 */
	private void writeObject(ObjectOutputStream out) throws IOException {
		out.defaultWriteObject();
		short alteredFieldsBitMask = getAlteredFieldsBitMask();
		out.writeShort(alteredFieldsBitMask);
		if (id != null) {
			out.writeObject(id);
		}
		if (startTimestamp != null) {
			out.writeObject(startTimestamp);
		}
		if (stopTimestamp != null) {
			out.writeObject(stopTimestamp);
		}
		if (lastAccessTime != null) {
			out.writeObject(lastAccessTime);
		}
		if (timeout != 0l) {
			out.writeLong(timeout);
		}
		if (expired) {
			out.writeBoolean(expired);
		}
		if (host != null) {
			out.writeUTF(host);
		}
	}

	/**
	 * Reconstitutes this object based on the specified InputStream for JDK
	 * Serialization.
	 *
	 * @param in
	 *            the input stream to use for reading data to populate this object.
	 * @throws IOException
	 *             if the input stream cannot be used.
	 * @throws ClassNotFoundException
	 *             if a required class needed for instantiation is not available in
	 *             the present JVM
	 */
	private void readObject(ObjectInputStream in) throws IOException, ClassNotFoundException {
		in.defaultReadObject();
		short bitMask = in.readShort();

		if (isFieldPresent(bitMask, ID_BIT_MASK)) {
			this.id = (Serializable) in.readObject();
		}
		if (isFieldPresent(bitMask, START_TIMESTAMP_BIT_MASK)) {
			this.startTimestamp = (Date) in.readObject();
		}
		if (isFieldPresent(bitMask, STOP_TIMESTAMP_BIT_MASK)) {
			this.stopTimestamp = (Date) in.readObject();
		}
		if (isFieldPresent(bitMask, LAST_ACCESS_TIME_BIT_MASK)) {
			this.lastAccessTime = (Date) in.readObject();
		}
		if (isFieldPresent(bitMask, TIMEOUT_BIT_MASK)) {
			this.timeout = in.readLong();
		}
		if (isFieldPresent(bitMask, EXPIRED_BIT_MASK)) {
			this.expired = in.readBoolean();
		}
		if (isFieldPresent(bitMask, HOST_BIT_MASK)) {
			this.host = in.readUTF();
		}
	}

	/**
	 * Returns a bit mask used during serialization indicating which fields have
	 * been serialized. Fields that have been altered (not null and/or not retaining
	 * the class defaults) will be serialized and have 1 in their respective index,
	 * fields that are null and/or retain class default values have 0.
	 *
	 * @return a bit mask used during serialization indicating which fields have
	 *         been serialized.
	 */
	private short getAlteredFieldsBitMask() {
		int bitMask = 0;
		bitMask = id != null ? bitMask | ID_BIT_MASK : bitMask;
		bitMask = startTimestamp != null ? bitMask | START_TIMESTAMP_BIT_MASK : bitMask;
		bitMask = stopTimestamp != null ? bitMask | STOP_TIMESTAMP_BIT_MASK : bitMask;
		bitMask = lastAccessTime != null ? bitMask | LAST_ACCESS_TIME_BIT_MASK : bitMask;
		bitMask = timeout != 0l ? bitMask | TIMEOUT_BIT_MASK : bitMask;
		bitMask = expired ? bitMask | EXPIRED_BIT_MASK : bitMask;
		bitMask = host != null ? bitMask | HOST_BIT_MASK : bitMask;
		return (short) bitMask;
	}

	/**
	 * Returns {@code true} if the given {@code bitMask} argument indicates that the
	 * specified field has been serialized and therefore should be read during
	 * deserialization, {@code false} otherwise.
	 *
	 * @param bitMask
	 *            the aggregate bitmask for all fields that have been serialized.
	 *            Individual bits represent the fields that have been serialized. A
	 *            bit set to 1 means that corresponding field has been serialized, 0
	 *            means it hasn't been serialized.
	 * @param fieldBitMask
	 *            the field bit mask constant identifying which bit to inspect
	 *            (corresponds to a class attribute).
	 * @return {@code true} if the given {@code bitMask} argument indicates that the
	 *         specified field has been serialized and therefore should be read
	 *         during deserialization, {@code false} otherwise.
	 */
	private static boolean isFieldPresent(short bitMask, int fieldBitMask) {
		return (bitMask & fieldBitMask) != 0;
	}

	/**
	 * Attributes are not used with this class
	 */
	@Override
	public Collection<Object> getAttributeKeys() throws InvalidSessionException {
		return null;
	}

	/**
	 * Attributes are not used with this class
	 */
	@Override
	public Object getAttribute(Object key) throws InvalidSessionException {
		return null;
	}

	/**
	 * Attributes are not used with this class
	 */
	@Override
	public void setAttribute(Object key, Object value) throws InvalidSessionException {
	}

	/**
	 * Attributes are not used with this class
	 */
	@Override
	public Object removeAttribute(Object key) throws InvalidSessionException {
		return null;
	}

}
