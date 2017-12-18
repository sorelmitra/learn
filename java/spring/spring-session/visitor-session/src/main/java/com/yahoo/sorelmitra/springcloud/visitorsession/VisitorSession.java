package com.yahoo.sorelmitra.springcloud.visitorsession;

import org.springframework.session.Session;

// Pointless interface, as VisitorSession and JdbcSession are on separate hierachies,
// and we can't extend from the latter
public interface VisitorSession extends Session {

	String getState();
	
}
