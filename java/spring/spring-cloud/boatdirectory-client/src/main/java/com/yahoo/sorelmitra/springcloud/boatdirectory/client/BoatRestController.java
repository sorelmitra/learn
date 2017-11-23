package com.yahoo.sorelmitra.springcloud.boatdirectory.client;

import java.util.List;

import org.apache.log4j.Logger;
import org.apache.tomcat.util.codec.binary.Base64;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cloud.client.ServiceInstance;
import org.springframework.cloud.client.discovery.DiscoveryClient;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.RestTemplate;

@RestController
@RequestMapping("/rest")
public class BoatRestController {
	private static Logger LOGGER = Logger.getLogger(BoatRestController.class);

	@Autowired
	private RestTemplate loadBalancedRestTemplate;
	
	@Value("${app.service.path}")
	private String servicePath;
	
	@GetMapping("/boats/names")
	public String[] getBoatNames() {
		HttpHeaders headers = prepareAuthorization();
		HttpEntity<String> request = new HttpEntity<String>(headers);
		
		try {
			LOGGER.info("URL: " + servicePath);
			ResponseEntity<String[]> response = loadBalancedRestTemplate.exchange(servicePath, HttpMethod.GET, request, String[].class);
			return response.getBody();
		} catch (RestClientException e) {
			LOGGER.error("ERROR", e);
			return new String[] {"ERROR: " + e.getMessage()};
		}
	}

	private HttpHeaders prepareAuthorization() {
		String plainCreds = "user:password";
		byte[] plainCredsBytes = plainCreds.getBytes();
		byte[] base64CredsBytes = Base64.encodeBase64(plainCredsBytes);
		String base64Creds = new String(base64CredsBytes);

		HttpHeaders headers = new HttpHeaders();
		headers.add("Authorization", "Basic " + base64Creds);
		return headers;
	}
}
