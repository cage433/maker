package starling.services.rpc

import org.scalatest.testng.TestNGSuite
import org.scalatest.matchers.ShouldMatchers
import org.testng.annotations.Test

// TODO - Fix Lift JSON for Day - no longer using local date in TitanSnapshotIdentifier
class JsonInteropTests extends TestNGSuite with ShouldMatchers {
  // Find interop json, instantiate class, round trip should produce same json
  @Test def overlappingClassesShouldProduceInteroperableJSON {
    JsonInteropValidator.validate("starling.api/resources", "com.trafigura.services")
  }
}
