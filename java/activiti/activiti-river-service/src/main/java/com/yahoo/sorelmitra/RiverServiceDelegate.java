package com.yahoo.sorelmitra;

import java.io.Serializable;
import java.util.Map;

public interface RiverServiceDelegate extends Serializable {

    void onBoatUnavailable(Map<String, Object> processInstanceVariables);

}
