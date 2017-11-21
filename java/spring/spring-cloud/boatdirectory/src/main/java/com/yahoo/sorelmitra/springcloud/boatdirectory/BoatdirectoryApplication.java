package com.yahoo.sorelmitra.springcloud.boatdirectory;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cloud.client.discovery.EnableDiscoveryClient;

@EnableDiscoveryClient
@SpringBootApplication
public class BoatdirectoryApplication {

    public static void main(String[] args) {
        SpringApplication.run(BoatdirectoryApplication.class, args);
    }
}
