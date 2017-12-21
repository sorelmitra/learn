package com.yahoo.sorelmitra.shiro.nomad;

import java.io.Serializable;
import java.util.List;

import org.springframework.data.repository.CrudRepository;
import org.springframework.stereotype.Component;

@Component
public interface NomadJpaRepositoryInterface extends CrudRepository<NomadSession, Serializable> {

	List<NomadSession> findByState(String state);

}
