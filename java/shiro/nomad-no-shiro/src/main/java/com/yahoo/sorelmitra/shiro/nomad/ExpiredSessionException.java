package com.yahoo.sorelmitra.shiro.nomad;

public class ExpiredSessionException extends Exception {

	/**
	 * 
	 */
	private static final long serialVersionUID = 282942411498128479L;

	public ExpiredSessionException(String msg) {
		super(msg);
	}
}
