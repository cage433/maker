package starling.schemaevolution

import system.Patch
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter
import starling.dbx.QueryBuilder._
import starling.market.MarketWriter

class Patch130_AddNewMarkets2 extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) {
    val markets = ss.split("\n").toList.map {
      case s => {
        MarketWriter.columns.zip(s.trim.split('\t')).toMap.mapValues{
          case "NULL" => null
          case o => o
        }
      }
    }
    writer.update("markets", Map("name" -> "Henry Financial LD1 Fixed Price (Swap)"), ("name" eql "Henry Financial LD1 Fixed Price"))
    writer.insert("markets", markets)
  }

  override def requiresRestart = true

  val ss = """CBOB NYH Barges (Argus) vs NYMEX RBOB 1st Month	1771	FormulaIndex	gal	USD	NULL	NULL	NULL	NULL	NULL	NULL	Some(4)	Some(4)	NULL	NULL	None	NULL	NULL	NULL	1.0* MKT(1770)- 1.0* MKT(933) 	NULL	NULL	Some(1.0)
CBOB NYH Barges (Argus)	1770	PublishedIndex	gal	USD	ARG	None	Day	Gasoline	Some(GP5168A0)	Some(0.01)	Some(4)	Some(4)	NULL	NULL	None	Mid	NULL	NULL	NULL	NULL	NULL	Some(1.0)
Zeebrugge Weekend ESGM (Heren)	1732	PublishedIndex	MT	GBP	<None>	None	Day	Crude	Some(ESGM.ZEEB)	Some(0.01)	Some(5)	Some(5)	NULL	NULL	None	FwdMid1wkn	NULL	NULL	NULL	NULL	NULL	Some(1.0)
Super Prem Unl USGC Pipeline [Low] (Platts)	1760	PublishedIndex	gal	USD	PLH	None	Day	Gasoline	Some(PGAJB00)	Some(0.01)	Some(4)	Some(4)	NULL	NULL	None	Low	NULL	NULL	NULL	NULL	NULL	Some(8.45)
No.6 3% USGC Waterborne vs ICE Brent 1st Month	1761	FormulaIndex	bbl	USD	NULL	NULL	NULL	NULL	NULL	NULL	Some(4)	Some(3)	NULL	NULL	None	NULL	NULL	NULL	1.0* MKT(11)- 1.0* MKT(28) 	NULL	NULL	Some(6.35)
TTF Day Ahead ESGM (Heren)	1728	PublishedIndex	MT	GBP	<None>	None	Day	Crude	Some(ESGM.TTF)	Some(0.01)	Some(5)	Some(5)	NULL	NULL	None	FwdMid1day	NULL	NULL	NULL	NULL	NULL	Some(1.0)
PEG Weekend ESGM (Heren)	1737	PublishedIndex	MT	GBP	<None>	None	Day	Crude	Some(ESGM.PEG)	Some(0.01)	Some(5)	Some(5)	NULL	NULL	None	FwdMid1wkn	NULL	NULL	NULL	NULL	NULL	Some(1.0)
Zeebrugge Day Ahead ESGM (Heren)	1725	PublishedIndex	MT	GBP	<None>	None	Day	Crude	Some(ESGM.ZEEB)	Some(0.01)	Some(5)	Some(5)	NULL	NULL	None	FwdMid1day	NULL	NULL	NULL	NULL	NULL	Some(1.0)
NBP Monthly Indices ESGM Report (Heren)	1739	PublishedIndex	MT	GBP	<None>	None	Day	Crude	Some(ESGM.PEG)	Some(0.01)	Some(5)	Some(5)	NULL	NULL	None	FwdMid1wkn	NULL	NULL	NULL	NULL	NULL	Some(1.0)
US CME (B100) vs CBOT Soybean Oil	1765	PublishedIndex	lb	USD	NYM	None	Day	VegetableOil	None	None	Some(3)	Some(3)	NULL	NULL	None	Close	NULL	NULL	NULL	NULL	NULL	Some(7.0)
US SME (B100) vs CBOT Soybean Oil	1766	PublishedIndex	lb	USD	NYM	None	Day	VegetableOil	None	None	Some(3)	Some(3)	NULL	NULL	None	Close	NULL	NULL	NULL	NULL	NULL	Some(7.0)
Panhandle FOM (Platts)	1772	PublishedIndex	MT	USD	PLH	None	Day	NatGas	Some(TRAF.NG.NA.PANHANDLE.FOM)	Some(1.0)	Some(6)	Some(6)	NULL	NULL	None	Close	NULL	NULL	NULL	NULL	NULL	Some(1.0)
NBP Weekend ESGM (Heren)	1723	PublishedIndex	MT	GBP	<None>	None	Day	Crude	Some(ESGM.NBP)	Some(0.01)	Some(5)	Some(5)	NULL	NULL	None	FwdMid1wkn	NULL	NULL	NULL	NULL	NULL	Some(1.0)
TTF Weekend ESGM (Heren)	1735	PublishedIndex	MT	GBP	<None>	None	Day	Crude	Some(ESGM.TTF)	Some(0.01)	Some(5)	Some(5)	NULL	NULL	None	FwdMid1wkn	NULL	NULL	NULL	NULL	NULL	Some(1.0)
Unl 87 CBOB USGC Pipeline (Platts)	1774	PublishedIndex	gal	USD	PLH	None	Day	Gasoline	Some(AARQU00)	Some(0.01)	Some(4)	Some(4)	NULL	NULL	None	Mid	NULL	NULL	NULL	NULL	NULL	Some(8.33)
NGPL South Texas GDA (Platts)	1769	PublishedIndex	MT	USD	PLH	None	Day	NatGas	Some(TRAF.NG.NA.NGPLSOUTHTEXAS.DAILY)	Some(1.0)	Some(6)	Some(6)	NULL	NULL	None	Close	NULL	NULL	NULL	NULL	NULL	Some(1.0)
TTF Month Ahead ESGM (Heren)	1736	PublishedIndex	MT	GBP	<None>	None	Day	Crude	Some(ESGM.TTF)	Some(0.01)	Some(5)	Some(5)	NULL	NULL	None	FwdMid01mo	NULL	NULL	NULL	NULL	NULL	Some(1.0)
NGPL South Texas Basis FOM (Platts)	1768	PublishedIndex	MT	USD	PLH	None	Day	NatGas	Some(TRAF.NG.NA.NGPLSOUTHTEXAS.BASIS)	Some(1.0)	Some(6)	Some(6)	NULL	NULL	None	Close	NULL	NULL	NULL	NULL	NULL	Some(1.0)
MichCon Basis FOM (Platts)	1767	PublishedIndex	MT	USD	PLH	None	Day	NatGas	Some(TRAF.NG.NA.MICHCON.BASIS)	Some(1.0)	Some(6)	Some(6)	NULL	NULL	None	Close	NULL	NULL	NULL	NULL	NULL	Some(1.0)
Unl 87 USGC Pipeline (Argus) vs NYMEX RBOB	1762	FormulaIndex	gal	USD	NULL	NULL	NULL	NULL	NULL	NULL	Some(4)	Some(4)	NULL	NULL	None	NULL	NULL	NULL	1.0* MKT(1482)- 1.0* MKT(880)- 	NULL	NULL	Some(8.33)
US CME (B100) vs NYMEX Heat	1764	PublishedIndex	gal	USD	NYM	None	Day	VegetableOil	None	None	Some(3)	Some(3)	NULL	NULL	None	Close	NULL	NULL	NULL	NULL	NULL	Some(7.45)
NBP Monthly Cumulative Index ESGM Report (Heren)	1742	PublishedIndex	MT	GBP	<None>	None	Day	Crude	Some(HERINX.WDAY)	Some(0.01)	Some(5)	Some(5)	NULL	NULL	None	PencePerTherm	NULL	NULL	NULL	NULL	NULL	Some(1.0)
Panhandle FOM Basis (Platts)	1773	PublishedIndex	MT	USD	PLH	None	Day	NatGas	Some(TRAF.NG.NA.PANHANDLE.BASIS)	Some(1.0)	Some(6)	Some(6)	NULL	NULL	None	Close	NULL	NULL	NULL	NULL	NULL	Some(1.0)
Heren Within Day Index (Pence per Therm) ESGM	1740	PublishedIndex	MT	GBP	<None>	None	Day	Crude	Some(ESGM.PEG)	Some(0.01)	Some(5)	Some(5)	NULL	NULL	None	FwdMid01mo	NULL	NULL	NULL	NULL	NULL	Some(1.0)
NBP Day Ahead ESGM (Heren)	1722	PublishedIndex	MT	GBP	<None>	None	Day	Crude	Some(ESGM.NBP)	Some(0.01)	Some(5)	Some(5)	NULL	NULL	None	FwdMid1day	NULL	NULL	NULL	NULL	NULL	Some(1.0)
Zeebrugge Month Ahead ESGM (Heren)	1733	PublishedIndex	MT	GBP	<None>	None	Day	Crude	Some(ESGM.ZEEB)	Some(0.01)	Some(5)	Some(5)	NULL	NULL	None	FwdMid01mo	NULL	NULL	NULL	NULL	NULL	Some(1.0)
NBP Month Ahead ESGM (Heren)	1724	PublishedIndex	MT	GBP	<None>	None	Day	Crude	Some(ESGM.NBP)	Some(0.01)	Some(5)	Some(5)	NULL	NULL	None	FwdMid01mo	NULL	NULL	NULL	NULL	NULL	Some(1.0)
US SME (B100) vs NYMEX Heat	1763	PublishedIndex	gal	USD	NYM	None	Day	VegetableOil	None	None	Some(3)	Some(3)	NULL	NULL	None	Close	NULL	NULL	NULL	NULL	NULL	Some(7.45)
PEG Month Ahead ESGM (Heren)	1738	PublishedIndex	MT	GBP	<None>	None	Day	Crude	Some(ESGM.PEG)	Some(0.01)	Some(5)	Some(5)	NULL	NULL	None	FwdMid01mo	NULL	NULL	NULL	NULL	NULL	Some(1.0)
Unl 87 CBOB USGC Pipeline (Platts) vs NYMEX RBOB 1st month	1775	FormulaIndex	gal	USD	NULL	NULL	NULL	NULL	NULL	NULL	Some(4)	Some(5)	NULL	NULL	None	NULL	NULL	NULL	1.0* MKT(1774)- 1.0* MKT(933) 	NULL	NULL	Some(8.33)
PEG Day Ahead ESGM (Heren)	1729	PublishedIndex	MT	GBP	<None>	None	Day	Crude	Some(ESGM.PEG)	Some(0.01)	Some(5)	Some(5)	NULL	NULL	None	FwdMid1day	NULL	NULL	NULL	NULL	NULL	Some(1.0)
Henry Financial LD1 Fixed Price	1677	FuturesMarket	MT	USD	NYM	Some(25000.0)	Month	NatGas	Some(NG)	Some(0.1)	Some(6)	Some(6)	No Rule	ICE	None	NULL	NULL	NULL	NULL	NULL	NULL	None
"""


}
