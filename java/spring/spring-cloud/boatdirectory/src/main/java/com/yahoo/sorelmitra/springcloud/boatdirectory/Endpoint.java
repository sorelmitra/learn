package com.yahoo.sorelmitra.springcloud.boatdirectory;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import org.springframework.stereotype.Component;

@Component
@Path("/boats")
public class Endpoint {

    @Path("/names")
    @Produces({ MediaType.APPLICATION_JSON })
    @GET
    public String[] boatsNames() {
        return new String[] { "HMS Belfast", "Cutty Sark" };
    }
}
