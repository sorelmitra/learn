package com.yahoo.sorelmitra.springmicroservices.ch2.bootmessaging;

import org.springframework.amqp.core.Queue;
import org.springframework.amqp.rabbit.core.RabbitMessagingTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.stereotype.Component;

@Component
public class Sender {
	@Autowired
	private RabbitMessagingTemplate rabbitMessagingTemplate;
	
	@Bean
	Queue queue() {
		return new Queue("TestQ");
	}
	
	public void send(String messasge) {
		rabbitMessagingTemplate.convertAndSend("TestQ", messasge);
	}
}
