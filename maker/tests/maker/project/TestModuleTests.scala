package maker.project

import org.scalatest.{Matchers, FreeSpec}
import maker.utils.FileUtils._
import maker.utils.os.Command

class TestModuleTests extends FreeSpec with Matchers{

  "application.conf should be picked up" in {
    withTestDir{
      dir => 
        TestModuleBuilder.createMakerProjectFile(dir)
        TestModuleBuilder.writeLogbackConfig(dir, "WARN")
        TestModuleBuilder.writeApplicationConfig(dir, "maker.http.proxy.port=9999")
        val module = new TestModuleBuilder(dir, "TestModuleTests").withExtraCode(
          s"""|  def checkConfig{
              |
              |    import com.typesafe.config.{Config, ConfigFactory}
              |
              |    val config = ConfigFactory.load()
              |
              |    assert(!config.getBoolean("maker.http.proxy.required"), "Proxy should be off by default")
              |    assert(config.getInt("maker.http.proxy.port") == 9999, "proxy port should be overriden")
              |  }""".stripMargin
        )
        module.appendDefinitionToProjectFile(dir)


        val makerScript = file("maker.py").getAbsolutePath
        val command = Command(
          "python",
          makerScript,
          "-e",
          file(dir, "config").absPath,
          "-E",
          "TestModuleTests.checkConfig",
          "-z",
          "-l",
          file(dir, "logback.xml").getAbsolutePath,
          "-L",
          "40"
        ).
        withWorkingDirectory(dir).
        withExitValues(0, 1).
        withNoOutput

        val result = command.run
        result should equal (0)
    }
  }
}
