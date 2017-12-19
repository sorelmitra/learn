package com.yahoo.sorelmitra.shiro.nomad;

import org.apache.shiro.mgt.DefaultSecurityManager;
import org.apache.shiro.mgt.SecurityManager;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class ShiroConfiguration {

	@Bean
	public SecurityManager securityManager() {
		DefaultSecurityManager securityManager = new DefaultSecurityManager();
		return securityManager;
	}

}
