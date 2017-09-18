package com.yahoo.sorelmitra.springmicroservices.ch2.bootmessaging;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

@SpringBootApplication
public class BootmessagingApplication implements CommandLineRunner {

	@Autowired
	Sender sender;
	
	public static void main(String[] args) {
		SpringApplication.run(BootmessagingApplication.class, args);
	}
	
	@Override
	public void run(String... arg0) throws Exception {
		sender.send("Spring Boot Messaging says Hi!");
	}
}
