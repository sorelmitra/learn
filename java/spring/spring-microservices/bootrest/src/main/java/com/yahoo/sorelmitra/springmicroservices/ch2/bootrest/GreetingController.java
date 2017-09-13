package com.yahoo.sorelmitra.springmicroservices.ch2.bootrest;

import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class GreetingController {
	@RequestMapping
	public Greet greet() {
		return new Greet();
	}
}
