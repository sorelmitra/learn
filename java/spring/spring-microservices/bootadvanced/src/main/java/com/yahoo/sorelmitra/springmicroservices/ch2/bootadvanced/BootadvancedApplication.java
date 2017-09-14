package com.yahoo.sorelmitra.springmicroservices.ch2.bootadvanced;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.security.config.annotation.method.configuration.EnableGlobalMethodSecurity;

@EnableGlobalMethodSecurity
@SpringBootApplication
public class BootadvancedApplication {

	public static void main(String[] args) {
		SpringApplication.run(BootadvancedApplication.class, args);
	}
}
