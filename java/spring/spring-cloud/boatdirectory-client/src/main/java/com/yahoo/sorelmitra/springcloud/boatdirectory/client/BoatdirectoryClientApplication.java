package com.yahoo.sorelmitra.springcloud.boatdirectory.client;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.builder.SpringApplicationBuilder;
import org.springframework.boot.web.support.SpringBootServletInitializer;

@SpringBootApplication
public class BoatdirectoryClientApplication extends SpringBootServletInitializer {

	@Override
	protected SpringApplicationBuilder configure(SpringApplicationBuilder builder) {
		return builder.sources(BoatdirectoryClientApplication.class);
	}

	public static void main(String[] args) {
		SpringApplication.run(BoatdirectoryClientApplication.class, args);
	}
}
