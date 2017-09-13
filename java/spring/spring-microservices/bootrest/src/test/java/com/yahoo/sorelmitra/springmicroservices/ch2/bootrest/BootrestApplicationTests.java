package com.yahoo.sorelmitra.springmicroservices.ch2.bootrest;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.web.client.RestTemplate;

@RunWith(SpringRunner.class)
@SpringBootTest
public class BootrestApplicationTests {

	@Test
	public void testGreeting() {
		RestTemplate restTemplate = new RestTemplate();
		Greet greet = restTemplate.getForObject("http://localhost:8080", Greet.class);
		Assert.assertEquals("Greetings, you", greet.getMessage());
		Assert.assertEquals("Visitor", greet.getRecipient());
	}

}
