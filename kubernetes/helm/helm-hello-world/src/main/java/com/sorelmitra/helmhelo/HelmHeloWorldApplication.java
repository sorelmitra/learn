package com.sorelmitra.helmhelo;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

@SpringBootApplication
public class HelmHeloWorldApplication {

	static private Logger LOG = LoggerFactory.getLogger(HelmHeloWorldApplication.class);

	public static void main(String[] args) {
		SpringApplication.run(HelmHeloWorldApplication.class, args);
	}

}
