package com.yahoo.sorelmitra.shiro.nomad;

import java.io.Serializable;

import org.springframework.data.repository.CrudRepository;
import org.springframework.stereotype.Component;

@Component
public interface NomadJpaRepositoryInterface extends CrudRepository<NomadSession, Serializable> {

}
