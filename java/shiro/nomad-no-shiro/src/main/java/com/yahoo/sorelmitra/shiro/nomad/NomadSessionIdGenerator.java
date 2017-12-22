package com.yahoo.sorelmitra.shiro.nomad;

import java.io.Serializable;

import org.springframework.stereotype.Component;

@Component
public class NomadSessionIdGenerator {

	private static int id = 0;

	public Serializable generateId(NomadSession arg0) {
		id++;
		return Integer.toString(id);
	}

}
