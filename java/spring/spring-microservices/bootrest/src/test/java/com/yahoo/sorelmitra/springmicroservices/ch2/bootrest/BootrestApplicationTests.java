package com.yahoo.sorelmitra.springmicroservices.ch2.bootrest;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.context.SpringBootTest.WebEnvironment;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.test.context.junit4.SpringRunner;

@RunWith(SpringRunner.class)
@SpringBootTest(webEnvironment = WebEnvironment.RANDOM_PORT)
public class BootrestApplicationTests {
	@Autowired
	TestRestTemplate restTemplate;
	
	@Test
	public void testGreeting() {
		Greet greet = restTemplate.getForObject("/", Greet.class);
		Assert.assertEquals("Greetings, you", greet.getMessage());
		Assert.assertEquals("Visitor", greet.getRecipient());
	}

}
