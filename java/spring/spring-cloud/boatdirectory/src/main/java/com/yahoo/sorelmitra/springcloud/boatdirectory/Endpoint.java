package com.yahoo.sorelmitra.springcloud.boatdirectory;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import org.apache.log4j.Logger;
import org.springframework.stereotype.Component;

@Component
@Path("/boats")
public class Endpoint {
	private static Logger LOGGER = Logger.getLogger(Endpoint.class);


    @Path("/names")
    @Produces({ MediaType.APPLICATION_JSON })
    @GET
    public String[] boatsNames() {
    		LOGGER.info("called");
        return new String[] { "HMS Belfast", "Cutty Sark" };
    }
}
