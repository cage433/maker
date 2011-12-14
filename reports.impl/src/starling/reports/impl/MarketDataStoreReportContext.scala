package starling.reports.impl

import pivot.CurveIdentifier
import starling.curves.{RecordingMarketDataReader, EnvironmentSlider}
import starling.db.{NormalMarketDataReader, MarketDataStore}
import starling.utils.ImplicitConversions._
import starling.daterange.{DayAndTime, Day}


class MarketDataStoreReportContext(
        db:MarketDataStore,
        curveIdentifier:CurveIdentifier,
        environmentSliders:List[EnvironmentSlider]) extends AbstractReportContext(curveIdentifier, environmentSliders) {

  lazy val recordingReader = new RecordingMarketDataReader(new NormalMarketDataReader(db, curveIdentifier.marketDataIdentifier))

  def recorded = recordingReader.recorded.toList.mapFirst(_.asTuple).map(_.flatten).toSet

  def atomicEnvironment(day:DayAndTime) = curveIdentifier.environmentRule.createEnv(day, recordingReader).environment.atomicEnv
}