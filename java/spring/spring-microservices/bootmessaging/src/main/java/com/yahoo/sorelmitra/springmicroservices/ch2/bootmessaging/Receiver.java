package com.yahoo.sorelmitra.springmicroservices.ch2.bootmessaging;

import org.springframework.amqp.rabbit.annotation.RabbitListener;
import org.springframework.stereotype.Component;

@Component
public class Receiver {
	@RabbitListener(queues = "TestQ")
	public void processMessage(String content) {
		System.out.println("Received content <" + content + ">");
	}
}
