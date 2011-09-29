package starling.reports.impl

import pivot.CurveIdentifier
import starling.curves.{RecordingMarketDataReader, EnvironmentSlider}
import starling.db.{NormalMarketDataReader, MarketDataStore}
import starling.daterange.Day
import starling.utils.ImplicitConversions._


class MarketDataStoreReportContext(
        db:MarketDataStore,
        curveIdentifier:CurveIdentifier,
        environmentSliders:List[EnvironmentSlider]) extends AbstractReportContext(curveIdentifier, environmentSliders) {

  lazy val recordingReader = new RecordingMarketDataReader(new NormalMarketDataReader(db, curveIdentifier.marketDataIdentifier))

  def recorded = recordingReader.recorded.toList.mapFirst(_.asTuple).map(_.flatten).toSet

  def atomicEnvironment(day:Day) = curveIdentifier.environmentRule.createEnv(day, recordingReader).environment.atomicEnv
}