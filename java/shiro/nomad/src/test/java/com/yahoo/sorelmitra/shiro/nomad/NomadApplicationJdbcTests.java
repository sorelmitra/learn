package com.yahoo.sorelmitra.shiro.nomad;

import org.junit.runner.RunWith;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit4.SpringRunner;

@RunWith(SpringRunner.class)
@SpringBootTest
@ActiveProfiles(profiles = "jdbc")
public class NomadApplicationJdbcTests extends NomadApplicationTests {

}
