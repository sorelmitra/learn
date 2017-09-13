package com.yahoo.sorelmitra.springmicroservices.ch2.boothateoas;

import org.springframework.hateoas.ResourceSupport;

public class Greet extends ResourceSupport {
	private String message;
	private String recipient;
	
	public Greet() {
		setMessage("HATEOAS Greetings, dear");
		setRecipient("Visitor");
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public String getRecipient() {
		return recipient;
	}

	public void setRecipient(String recipient) {
		this.recipient = recipient;
	}
}
