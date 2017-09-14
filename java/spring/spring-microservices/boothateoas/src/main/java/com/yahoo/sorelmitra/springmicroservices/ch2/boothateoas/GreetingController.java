package com.yahoo.sorelmitra.springmicroservices.ch2.boothateoas;

import static org.springframework.hateoas.mvc.ControllerLinkBuilder.linkTo;
import static org.springframework.hateoas.mvc.ControllerLinkBuilder.methodOn;

import java.net.MalformedURLException;
import java.net.URL;

import org.springframework.hateoas.Link;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class GreetingController {

	@RequestMapping("/greeting")
	@ResponseBody
	public HttpEntity<Greet> greeting(@RequestParam(value = "name", required = false, defaultValue = "HATEOAS") String name) throws NoSuchMethodException, SecurityException, MalformedURLException {
		Greet greet = new Greet();
		greet.setRecipient(name);
		Link selfLinkBuilder = linkTo(methodOn(GreetingController.class).greeting(name)).withSelfRel();
		greet.add(selfLinkBuilder);
		String siblingName = "stuff";
		String link = createSiblingLink(selfLinkBuilder, siblingName);
		greet.add(new Link(link, siblingName));
		return new ResponseEntity<Greet>(greet, HttpStatus.OK);
	}

	private String createSiblingLink(Link selfLinkBuilder, String siblingName) throws MalformedURLException {
		URL url = new URL(selfLinkBuilder.getHref());
		String path = url.getFile().substring(0, url.getFile().lastIndexOf('/'));
		String basePath = (url.getProtocol() + "://" + url.getHost() + ":" + url.getPort() + path + "/").toString();
		String link = basePath + siblingName;
		return link;
	}

	@RequestMapping("/stuff")
	@ResponseBody
	public HttpEntity<Stuff> doStuff() {
		Stuff stuff = new Stuff();
		stuff.setStuff("I'm doing stuff");
		return new ResponseEntity<Stuff>(stuff, HttpStatus.OK);
	}
}
