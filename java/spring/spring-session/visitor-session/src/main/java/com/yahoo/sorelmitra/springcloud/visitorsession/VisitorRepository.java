package com.yahoo.sorelmitra.springcloud.visitorsession;

import org.springframework.session.Session;
import org.springframework.session.SessionRepository;
import org.springframework.stereotype.Component;

@Component
public class VisitorRepository<S extends Session> {
	private SessionRepository<S> repository;

	public String saveSession() {
		S toSave = getRepository().createSession();

		toSave.setAttribute("id", "12");
		toSave.setAttribute("state", "INCOMING");
		toSave.setAttribute("last_msg_timestamp", "2017-12-14 15:22:00");

		getRepository().save(toSave);

		return toSave.getId();
	}

	public SessionRepository<S> getRepository() {
		return repository;
	}

	public void setRepository(SessionRepository<S> repository) {
		this.repository = repository;
	}
}