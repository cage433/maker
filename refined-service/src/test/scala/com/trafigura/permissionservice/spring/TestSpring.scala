package com.trafigura.permissionservice.spring

import org.springframework.context.support.ClassPathXmlApplicationContext
import org.testng.annotations.Test

class TestSpring {
  // can we load the configuration with test datasources?
  @Test(enabled=true)
  def testConfiguration : Unit = {
//    new ClassPathXmlApplicationContext("starling.xml","ds-test.xml")
  }
}
