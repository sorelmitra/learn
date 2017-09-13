package com.yahoo.sorelmitra.springmicroservices.ch2.bootrest;

public class Greet {
	private String message;
	private String recipient;
	
	public Greet() {
		setMessage("Greetings, you");
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
