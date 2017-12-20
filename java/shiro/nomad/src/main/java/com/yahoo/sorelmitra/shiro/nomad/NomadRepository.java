package com.yahoo.sorelmitra.shiro.nomad;

import java.io.Serializable;

import org.springframework.data.repository.CrudRepository;
import org.springframework.stereotype.Component;

@Component
public interface NomadRepository extends CrudRepository<NomadSession, Serializable> {

}
