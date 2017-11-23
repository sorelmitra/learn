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

	RestTemplate restTemplate = new RestTemplate();
	
	@Autowired
	DiscoveryClient discoveryClient;
	
	@Value("${app.service.name}")
	String serviceName;
	
	@Value("${app.service.path}")
	String servicePath;
	
	@GetMapping("/boats/names")
	public String[] getBoatNames() {
		HttpHeaders headers = prepareAuthorization();
		HttpEntity<String> request = new HttpEntity<String>(headers);
		
		try {
			String url = getServiceUrl();
			LOGGER.info("URL: " + url);
			ResponseEntity<String[]> response = restTemplate.exchange(url, HttpMethod.GET, request, String[].class);
			return response.getBody();
		} catch (RestClientException | ServiceException e) {
			LOGGER.error("ERROR", e);
			return new String[] {"ERROR: " + e.getMessage()};
		}
	}

	private String getServiceUrl() throws ServiceException {
		List<ServiceInstance> serviceInstances = discoveryClient.getInstances(serviceName);
		if (serviceInstances.size() < 1) {
			throw new ServiceException("Could not find in Consul the service with name " + serviceName);
		}
		ServiceInstance serviceInstance = serviceInstances.get(0);
		String url = serviceInstance.getUri().toString() + servicePath;
		return url;
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
