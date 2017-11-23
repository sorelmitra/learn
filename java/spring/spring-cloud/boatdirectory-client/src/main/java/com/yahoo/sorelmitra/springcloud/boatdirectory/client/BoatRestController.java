package com.yahoo.sorelmitra.springcloud.boatdirectory.client;

import org.apache.tomcat.util.codec.binary.Base64;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.client.RestTemplate;

@RestController
@RequestMapping("/rest")
public class BoatRestController {

	RestTemplate restTemplate = new RestTemplate();
	
	@GetMapping("/boats/names")
	public String[] getBoatNames() {
		String plainCreds = "user:password";
		byte[] plainCredsBytes = plainCreds.getBytes();
		byte[] base64CredsBytes = Base64.encodeBase64(plainCredsBytes);
		String base64Creds = new String(base64CredsBytes);

		HttpHeaders headers = new HttpHeaders();
		headers.add("Authorization", "Basic " + base64Creds);

		String url = "http://localhost:8770/api/boats/names";
		HttpEntity<String> request = new HttpEntity<String>(headers);
		ResponseEntity<String[]> response = restTemplate.exchange(url, HttpMethod.GET, request, String[].class);
		return response.getBody();
	}
}
