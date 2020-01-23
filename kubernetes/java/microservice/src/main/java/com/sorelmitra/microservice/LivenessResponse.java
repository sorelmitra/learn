package com.sorelmitra.microservice;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class LivenessResponse {

    private static final Logger LOG = LoggerFactory.getLogger(LivenessResponse.class);

    @Autowired
    MicroService ms;

    @GetMapping("/healthz")
    public ResponseEntity<String> healthz() {
        HttpStatus status = HttpStatus.NOT_FOUND;
        String response = Constants.DEAD;
        if (ms.areAllThreadsAlive()) {
            status = HttpStatus.OK;
            response = Constants.ALIVE;
        }
        LOG.info("Returning {} - {}", status, response);
        return ResponseEntity.status(status).body(response);
    }

}
