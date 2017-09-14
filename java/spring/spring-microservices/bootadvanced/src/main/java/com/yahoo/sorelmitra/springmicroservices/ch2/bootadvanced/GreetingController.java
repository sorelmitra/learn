package com.yahoo.sorelmitra.springmicroservices.ch2.bootadvanced;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class GreetingController {
	@Autowired
	Environment env;
	
	@RequestMapping
	public Greet greet() {
		Greet greet = new Greet();
		greet.setMessage(env.getProperty("greet.message"));
		greet.setRecipient(env.getProperty("greet.recipient"));
		return greet;
	}
}
