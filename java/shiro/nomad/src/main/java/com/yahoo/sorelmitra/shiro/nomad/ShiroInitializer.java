package com.yahoo.sorelmitra.shiro.nomad;

import javax.annotation.PostConstruct;

import org.apache.shiro.SecurityUtils;
import org.apache.shiro.mgt.SecurityManager;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ShiroInitializer {

	@Autowired
	SecurityManager securityManager;
	
	@PostConstruct
	public void init() {
		SecurityUtils.setSecurityManager(securityManager);
	}
}
