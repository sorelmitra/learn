package com.yahoo.sorelmitra.springmicroservices.ch2.boothateoas;

import org.springframework.hateoas.ResourceSupport;

public class Stuff extends ResourceSupport {
	private String stuff;

	public String getStuff() {
		return stuff;
	}

	public void setStuff(String stuff) {
		this.stuff = stuff;
	}
}
