package com.yahoo.sorelmitra.shiro.nomad;

import java.io.Serializable;

import org.springframework.data.repository.CrudRepository;

public interface NomadRepository extends CrudRepository<NomadSession, Serializable> {

}
