package starling.schemaevolution

import system.Patch
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter
import starling.gui.api.{EAIDeskInfo, Desk}
import starling.dbx.QueryBuilder._

class Patch123_AddNewMarkets1 extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) {
    val markets = ss.split("\n").toList.map {
      case s => {
        s.trim.split('\t').map {
          e =>
            e.trim.split("::").toList match {
              case k :: v :: Nil => (k -> v)
            }
        }.toMap
      }
    }
    starling.inTransaction {
      writer => {
        writer.queryForUpdate(select ("*") from "markets") {
          rs => {
            val quoteid = rs.getFromOption[Int]("eaiQuoteID")
            rs.update(Map("eaiQuoteID" -> quoteid.getOrElse(null)))
          }
        }

        writer.update("markets", Map("name" -> "HSFO 180 CST FOB AG Cargoes (6.5) [High] (Platts)"), ("name" eql "HSFO 180 CST FOB AG Cargoes [High] (Platts)"))
        writer.update("markets", Map("name" -> "HSFO 180 CST FOB AG Cargoes (6.5) (Platts)"), ("name" eql "HSFO 180 CST FOB AG Cargoes (Platts)"))
        writer.update("markets", Map("name" -> "HSFO 180 CST FOB AG Cargoes (6.5) [Low] (Platts)"), ("name" eql "HSFO 180 CST FOB AG Cargoes [Low] (Platts)"))
        writer.update("markets", Map("name" -> "HSFO 380 CST FOB AG Cargoes (6.5) (Platts)"), ("name" eql "HSFO 380 CST FOB AG Cargoes (Platts)"))
        writer.update("markets", Map("name" -> "HSFO 180 CST Singapore (6.5) vs Platts Dubai 1st month"), ("name" eql "HSFO 180 CST Singapore vs Platts Dubai 1st month"))
        writer.update("markets", Map("name" -> "HSFO 180 CST Singapore (6.5)"), ("name" eql "HSFO 180 CST Singapore"))
        writer.insert("markets", markets)
      }
    }
  }

  override def requiresRestart = true

  val ss = """eaiquoteid::1678	name::Mich Con GDA (Platts)	tenor::Day	bblPerMT::Some(1.0)	limMultiplier::Some(0.1)	businessCalendar::PLH	lotSize::None	indexlevel::Mid	volatilityID::None	limSymbol::Some(IGBDZ21)	clearPortPrecision::Some(6)	uom::MT	type::PublishedIndex	ccy::USD	defaultPrecision::Some(6)	commodity::NatGas
eaiquoteid::1679	name::NGPL STX GDA (Platts)	tenor::Day	bblPerMT::Some(1.0)	limMultiplier::Some(0.1)	businessCalendar::PLH	lotSize::None	indexlevel::Mid	volatilityID::None	limSymbol::Some(IGBAZ21)	clearPortPrecision::Some(6)	uom::MT	type::PublishedIndex	ccy::USD	defaultPrecision::Some(6)	commodity::NatGas
eaiquoteid::1680	name::NGPL TXOK GDA (Platts)	tenor::Day	bblPerMT::Some(1.0)	limMultiplier::Some(0.1)	businessCalendar::PLH	lotSize::None	indexlevel::Mid	volatilityID::None	limSymbol::Some(IGBAL21)	clearPortPrecision::Some(6)	uom::MT	type::PublishedIndex	ccy::USD	defaultPrecision::Some(6)	commodity::NatGas
eaiquoteid::1681	name::Panhandle GDA (Platts)	tenor::Day	bblPerMT::Some(1.0)	limMultiplier::Some(0.1)	businessCalendar::PLH	lotSize::None	indexlevel::Mid	volatilityID::None	limSymbol::Some(IGBCE21)	clearPortPrecision::Some(6)	uom::MT	type::PublishedIndex	ccy::USD	defaultPrecision::Some(6)	commodity::NatGas
eaiquoteid::1682	name::ANR SE (LA) GDA (Platts)	tenor::Day	bblPerMT::Some(1.0)	limMultiplier::Some(0.1)	businessCalendar::PLH	lotSize::None	indexlevel::Mid	volatilityID::None	limSymbol::Some(IGBBF21)	clearPortPrecision::Some(6)	uom::MT	type::PublishedIndex	ccy::USD	defaultPrecision::Some(6)	commodity::NatGas
eaiquoteid::1683	name::ANR SW GDA (Platts)	tenor::Day	bblPerMT::Some(1.0)	limMultiplier::Some(0.1)	businessCalendar::PLH	lotSize::None	indexlevel::Mid	volatilityID::None	limSymbol::Some(IGBBY21)	clearPortPrecision::Some(6)	uom::MT	type::PublishedIndex	ccy::USD	defaultPrecision::Some(6)	commodity::NatGas
eaiquoteid::1684	name::Chicago GDA (Platts)	tenor::Day	bblPerMT::Some(1.0)	limMultiplier::Some(0.1)	businessCalendar::PLH	lotSize::None	indexlevel::Mid	volatilityID::None	limSymbol::Some(IGBDX21)	clearPortPrecision::Some(6)	uom::MT	type::PublishedIndex	ccy::USD	defaultPrecision::Some(6)	commodity::NatGas
eaiquoteid::1685	name::Demarc GDA (Platts)	tenor::Day	bblPerMT::Some(1.0)	limMultiplier::Some(0.1)	businessCalendar::PLH	lotSize::None	indexlevel::Mid	volatilityID::None	limSymbol::Some(IGBDV21)	clearPortPrecision::Some(6)	uom::MT	type::PublishedIndex	ccy::USD	defaultPrecision::Some(6)	commodity::NatGas
eaiquoteid::1686	name::HSC GDA (Platts)	tenor::Day	bblPerMT::Some(1.0)	limMultiplier::Some(0.1)	businessCalendar::PLH	lotSize::None	indexlevel::Mid	volatilityID::None	limSymbol::Some(IGBAP21)	clearPortPrecision::Some(6)	uom::MT	type::PublishedIndex	ccy::USD	defaultPrecision::Some(6)	commodity::NatGas
eaiquoteid::1687	name::Henry Hub GDA (Platts)	tenor::Day	bblPerMT::Some(1.0)	limMultiplier::Some(0.1)	businessCalendar::PLH	lotSize::Some(10000.0)	indexlevel::Mid	volatilityID::None	limSymbol::Some(IGBBL21)	clearPortPrecision::Some(6)	uom::mmbtu	type::PublishedIndex	ccy::USD	defaultPrecision::Some(6)	commodity::NatGas
eaiquoteid::1688	name::HSFO 180 CST Singapore	tenor::Day	bblPerMT::Some(6.35)	limMultiplier::Some(1.0)	businessCalendar::PLD	lotSize::None	indexlevel::Mid	volatilityID::None	limSymbol::Some(PUADV00)	clearPortPrecision::Some(0)	uom::MT	type::PublishedIndex	ccy::USD	defaultPrecision::Some(4)	commodity::FuelOil
eaiquoteid::1689	name::HSFO 180 CST Singapore vs 3.5% Fuel FOB Rotterdam Barges	bblPerMT::Some(6.35)	formula::1.0* MKT(1688)- 1.0* MKT(5) 	volatilityID::None	clearPortPrecision::Some(0)	uom::MT	type::FormulaIndex	ccy::USD	defaultPrecision::Some(4)
eaiquoteid::1690	name::HSFO 180 CST FOB AG Cargoes [High] (Platts)	tenor::Day	bblPerMT::Some(6.35)	businessCalendar::PLD	lotSize::None	indexlevel::Close	volatilityID::None	clearPortPrecision::Some(0)	uom::MT	type::PublishedIndex	ccy::USD	defaultPrecision::Some(4)	commodity::FuelOil
eaiquoteid::1691	name::HSFO 180 CST FOB AG Cargoes (Platts)	tenor::Day	bblPerMT::Some(6.35)	businessCalendar::PLD	lotSize::None	indexlevel::Close	volatilityID::None	clearPortPrecision::Some(0)	uom::MT	type::PublishedIndex	ccy::USD	defaultPrecision::Some(4)	commodity::FuelOil
eaiquoteid::1692	name::HSFO 180 CST FOB AG Cargoes [Low] (Platts)	tenor::Day	bblPerMT::Some(6.35)	businessCalendar::PLD	lotSize::None	indexlevel::Close	volatilityID::None	clearPortPrecision::Some(0)	uom::MT	type::PublishedIndex	ccy::USD	defaultPrecision::Some(4)	commodity::FuelOil
eaiquoteid::1693	name::HSFO 380 CST FOB AG Cargoes (Platts)	tenor::Day	bblPerMT::Some(6.35)	businessCalendar::PLD	lotSize::None	indexlevel::Close	volatilityID::None	clearPortPrecision::Some(0)	uom::MT	type::PublishedIndex	ccy::USD	defaultPrecision::Some(4)	commodity::FuelOil
eaiquoteid::1694	name::HSFO 180 CST Singapore vs Platts Dubai 1st month	bblPerMT::Some(6.35)	formula::1.0* MKT(1688)- 1.0* MKT(6) 	volatilityID::None	clearPortPrecision::Some(0)	uom::bbl	type::FormulaIndex	ccy::USD	defaultPrecision::Some(4)
eaiquoteid::1695	name::HSFO 180 CST Singapore vs HSFO 380 CST Singapore	bblPerMT::Some(6.35)	formula::1.0* MKT(1688)- 1.0* MKT(134) 	volatilityID::None	clearPortPrecision::Some(0)	uom::MT	type::FormulaIndex	ccy::USD	defaultPrecision::Some(4)
eaiquoteid::1696	name::HSFO 180 CST Singapore vs No.6 3% USGC Waterborne	bblPerMT::Some(6.35)	formula::1.0* MKT(1688)- 1.0* MKT(11) 	volatilityID::None	clearPortPrecision::Some(0)	uom::MT	type::FormulaIndex	ccy::USD	defaultPrecision::Some(4)
eaiquoteid::1697	name::Mich Con GDA [Index] (Platts)	tenor::Day	bblPerMT::Some(1.0)	limMultiplier::Some(0.1)	businessCalendar::PLH	lotSize::None	indexlevel::Mid	volatilityID::None	limSymbol::Some(PUADV00)	clearPortPrecision::Some(6)	uom::MT	type::PublishedIndex	ccy::USD	defaultPrecision::Some(6)	commodity::NatGas
eaiquoteid::1698	name::NGPL STX GDA [Index] (Platts)	tenor::Day	bblPerMT::Some(1.0)	limMultiplier::Some(0.1)	businessCalendar::PLH	lotSize::None	indexlevel::Mid	volatilityID::None	limSymbol::Some(PUADV00)	clearPortPrecision::Some(6)	uom::MT	type::PublishedIndex	ccy::USD	defaultPrecision::Some(6)	commodity::NatGas
eaiquoteid::1699	name::NGPL TXOK GDA [Index] (Platts)	tenor::Day	bblPerMT::Some(1.0)	limMultiplier::Some(0.1)	businessCalendar::PLH	lotSize::None	indexlevel::Mid	volatilityID::None	limSymbol::Some(PUADV00)	clearPortPrecision::Some(6)	uom::MT	type::PublishedIndex	ccy::USD	defaultPrecision::Some(6)	commodity::NatGas
eaiquoteid::1700	name::Panhandle GDA [Index] (Platts)	tenor::Day	bblPerMT::Some(1.0)	limMultiplier::Some(0.1)	businessCalendar::PLH	lotSize::None	indexlevel::Mid	volatilityID::None	limSymbol::Some(PUADV00)	clearPortPrecision::Some(6)	uom::MT	type::PublishedIndex	ccy::USD	defaultPrecision::Some(6)	commodity::NatGas
eaiquoteid::1701	name::ANR SE (LA) GDA [Index] (Platts)	tenor::Day	bblPerMT::Some(1.0)	limMultiplier::Some(0.1)	businessCalendar::PLH	lotSize::None	indexlevel::Mid	volatilityID::None	limSymbol::Some(PUADV00)	clearPortPrecision::Some(6)	uom::MT	type::PublishedIndex	ccy::USD	defaultPrecision::Some(6)	commodity::NatGas
eaiquoteid::1702	name::ANR SW GDA [Index] (Platts)	tenor::Day	bblPerMT::Some(1.0)	limMultiplier::Some(0.1)	businessCalendar::PLH	lotSize::None	indexlevel::Index	volatilityID::None	limSymbol::Some(IGBBY21)	clearPortPrecision::Some(6)	uom::MT	type::PublishedIndex	ccy::USD	defaultPrecision::Some(6)	commodity::NatGas
eaiquoteid::1703	name::Chicago GDA [Index] (Platts)	tenor::Day	bblPerMT::Some(1.0)	limMultiplier::Some(0.1)	businessCalendar::PLH	lotSize::None	indexlevel::Mid	volatilityID::None	limSymbol::Some(PUADV00)	clearPortPrecision::Some(6)	uom::MT	type::PublishedIndex	ccy::USD	defaultPrecision::Some(6)	commodity::NatGas
eaiquoteid::1704	name::Demarc GDA [Index] (Platts)	tenor::Day	bblPerMT::Some(1.0)	limMultiplier::Some(0.1)	businessCalendar::PLH	lotSize::None	indexlevel::Mid	volatilityID::None	limSymbol::Some(PUADV00)	clearPortPrecision::Some(6)	uom::MT	type::PublishedIndex	ccy::USD	defaultPrecision::Some(6)	commodity::NatGas
eaiquoteid::1705	name::HSC GDA [Index] (Platts)	tenor::Day	bblPerMT::Some(1.0)	limMultiplier::Some(0.1)	businessCalendar::PLH	lotSize::None	indexlevel::Mid	volatilityID::None	limSymbol::Some(PUADV00)	clearPortPrecision::Some(6)	uom::MT	type::PublishedIndex	ccy::USD	defaultPrecision::Some(6)	commodity::NatGas
eaiquoteid::1706	name::Henry Hub GDA [Index] (Platts)	tenor::Day	bblPerMT::Some(1.0)	limMultiplier::Some(0.1)	businessCalendar::PLH	lotSize::Some(10000.0)	indexlevel::Mid	volatilityID::None	limSymbol::Some(PUADV00)	clearPortPrecision::Some(6)	uom::mmbtu	type::PublishedIndex	ccy::USD	defaultPrecision::Some(6)	commodity::NatGas
eaiquoteid::1707	name::NBP Daily Month Ahead Index (Heren)	tenor::Day	bblPerMT::Some(1.0)	limMultiplier::Some(0.01)	businessCalendar::<None>	lotSize::None	indexlevel::Close	volatilityID::None	limSymbol::Some(HEREN.NBP.DAILY.MONTH.AHEAD.INDEX)	clearPortPrecision::Some(0)	uom::MT	type::PublishedIndex	ccy::GBP	defaultPrecision::Some(5)	commodity::Crude
eaiquoteid::1708	name::TTF Daily Month Ahead Index (Heren)	tenor::Day	bblPerMT::Some(1.0)	limMultiplier::Some(0.01)	businessCalendar::<None>	lotSize::None	indexlevel::Close	volatilityID::None	limSymbol::Some(HEREN.TTF.DAILY.MONTH.AHEAD.INDEX)	clearPortPrecision::Some(0)	uom::MT	type::PublishedIndex	ccy::GBP	defaultPrecision::Some(5)	commodity::Crude
eaiquoteid::1709	name::Zeebrugge Daily Month Ahead Index (Heren)	tenor::Day	bblPerMT::Some(1.0)	limMultiplier::Some(0.01)	businessCalendar::<None>	lotSize::None	indexlevel::Close	volatilityID::None	limSymbol::Some(HEREN.ZEEBRUGGE.DAILY.MONTH.AHEAD.INDEX)	clearPortPrecision::Some(0)	uom::MT	type::PublishedIndex	ccy::GBP	defaultPrecision::Some(5)	commodity::Crude
eaiquoteid::1710	name::NCG Daily Month Ahead Index (Heren)	tenor::Day	bblPerMT::Some(1.0)	limMultiplier::Some(0.01)	businessCalendar::<None>	lotSize::None	indexlevel::Close	volatilityID::None	limSymbol::Some(HEREN.NCG.DAILY.MONTH.AHEAD.INDEX)	clearPortPrecision::Some(0)	uom::MT	type::PublishedIndex	ccy::GBP	defaultPrecision::Some(5)	commodity::Crude
eaiquoteid::1711	name::AB-Nit Monthly [Index] (Platts)	tenor::Day	bblPerMT::Some(1.0)	limMultiplier::Some(0.105506)	businessCalendar::PLH	lotSize::None	indexlevel::Index	volatilityID::None	limSymbol::Some(IGBCU03)	clearPortPrecision::Some(6)	uom::MT	type::PublishedIndex	ccy::CAD	defaultPrecision::Some(6)	commodity::NatGas
eaiquoteid::1712	name::Coal Colombian Drummond Small Vessel	tenor::Day	bblPerMT::None	businessCalendar::ARK	lotSize::None	indexlevel::Close	volatilityID::None	clearPortPrecision::Some(0)	uom::MT	type::PublishedIndex	ccy::USD	defaultPrecision::Some(3)	commodity::Coal
eaiquoteid::1713	name::Coal Nola Low Sulphur (<1%)	tenor::Day	bblPerMT::None	businessCalendar::ARK	lotSize::None	indexlevel::Close	volatilityID::None	clearPortPrecision::Some(0)	uom::MT	type::PublishedIndex	ccy::USD	defaultPrecision::Some(3)	commodity::Coal
eaiquoteid::1714	name::Asphalt FOB Thailand Cargoes (Argus)	tenor::Day	bblPerMT::Some(5.47)	limMultiplier::Some(1.0)	businessCalendar::ARK	lotSize::None	indexlevel::Mid	volatilityID::None	limSymbol::Some(GPAS0127)	clearPortPrecision::Some(0)	uom::MT	type::PublishedIndex	ccy::USD	defaultPrecision::Some(3)	commodity::GasOil
eaiquoteid::1715	name::Asphalt FOB Taiwan Cargoes (Argus)	tenor::Day	bblPerMT::Some(5.47)	limMultiplier::Some(1.0)	businessCalendar::ARK	lotSize::None	indexlevel::Mid	volatilityID::None	limSymbol::Some(GPAS0164)	clearPortPrecision::Some(0)	uom::MT	type::PublishedIndex	ccy::USD	defaultPrecision::Some(3)	commodity::GasOil
eaiquoteid::1716	name::Asphalt FOB South Korea Cargoes (Argus)	tenor::Day	bblPerMT::Some(5.47)	limMultiplier::Some(1.0)	businessCalendar::ARK	lotSize::None	indexlevel::Mid	volatilityID::None	limSymbol::Some(GPAS0164)	clearPortPrecision::Some(0)	uom::MT	type::PublishedIndex	ccy::USD	defaultPrecision::Some(3)	commodity::GasOil
eaiquoteid::1717	name::Asphalt South Korea - East China (Argus)	tenor::Day	bblPerMT::Some(5.47)	limMultiplier::Some(1.0)	businessCalendar::ARK	lotSize::None	indexlevel::Mid	volatilityID::None	limSymbol::Some(GPAS7138)	clearPortPrecision::Some(0)	uom::MT	type::PublishedIndex	ccy::USD	defaultPrecision::Some(3)	commodity::GasOil
eaiquoteid::1718	name::Asphalt Taiwan East - South China (Argus)	tenor::Day	bblPerMT::Some(5.47)	limMultiplier::Some(1.0)	businessCalendar::ARK	lotSize::None	indexlevel::Mid	volatilityID::None	limSymbol::Some(GPAS7137)	clearPortPrecision::Some(0)	uom::MT	type::PublishedIndex	ccy::USD	defaultPrecision::Some(3)	commodity::GasOil
eaiquoteid::1719	name::Asphalt Thailand - East China (Argus)	tenor::Day	bblPerMT::Some(5.47)	limMultiplier::Some(1.0)	businessCalendar::ARK	lotSize::None	indexlevel::Mid	volatilityID::None	limSymbol::Some(GPAS7136)	clearPortPrecision::Some(0)	uom::MT	type::PublishedIndex	ccy::USD	defaultPrecision::Some(3)	commodity::GasOil
eaiquoteid::1720	name::Asphalt Thailand - South China (Argus)	tenor::Day	bblPerMT::Some(5.47)	limMultiplier::Some(1.0)	businessCalendar::ARK	lotSize::None	indexlevel::Mid	volatilityID::None	limSymbol::Some(GPAS7135)	clearPortPrecision::Some(0)	uom::MT	type::PublishedIndex	ccy::USD	defaultPrecision::Some(3)	commodity::GasOil
eaiquoteid::1721	name::Hard Coking Coal (HCC 64 Mid Vol) FOB Australia (Platts)	tenor::Day	bblPerMT::None	limMultiplier::Some(1.0)	businessCalendar::NYM	lotSize::Some(1000.0)	indexlevel::Close	volatilityID::None	limSymbol::Some(HCCAU00)	clearPortPrecision::Some(2)	uom::MT	type::PublishedIndex	ccy::USD	defaultPrecision::Some(2)	commodity::Coal
eaiquoteid::1726	name::VGO-MS Barge diff to WTI (OPIS)	tenor::Day	bblPerMT::Some(7.0)	limMultiplier::Some(1.0)	businessCalendar::ARG	lotSize::None	indexlevel::MidPoint	volatilityID::None	limSymbol::Some(VGOMS.B.DIFF.TO.WTI)	clearPortPrecision::Some(0)	uom::bbl	type::PublishedIndex	ccy::USD	defaultPrecision::Some(4)	commodity::FuelOil
eaiquoteid::1727	name::VGO-HS Barge diff to WTI (OPIS)	tenor::Day	bblPerMT::Some(7.0)	limMultiplier::Some(1.0)	businessCalendar::ARG	lotSize::None	indexlevel::MidPoint	volatilityID::None	limSymbol::Some(VGOHS.B.DIFF.TO.WTI)	clearPortPrecision::Some(0)	uom::bbl	type::PublishedIndex	ccy::USD	defaultPrecision::Some(4)	commodity::FuelOil
eaiquoteid::1730	name::Naphtha Domestic DIFF (OPIS)	tenor::Day	bblPerMT::Some(8.9)	limMultiplier::Some(0.01)	businessCalendar::OPI	lotSize::None	indexlevel::Mid	volatilityID::None	limSymbol::Some(NAPHTHA.DOMD)	clearPortPrecision::Some(0)	uom::gal	type::PublishedIndex	ccy::USD	defaultPrecision::Some(4)	commodity::GasOil
eaiquoteid::1731	name::TC12 West Coast India to Japan (Baltic)	tenor::Day	bblPerMT::None	limMultiplier::Some(0.01)	businessCalendar::IcS	lotSize::Some(1000.0)	indexlevel::Val	volatilityID::None	limSymbol::Some(BALTIC.CLEAN.TANKER.ROUTE.TC12)	clearPortPrecision::Some(0)	uom::MT	type::PublishedIndex	ccy::WSC	defaultPrecision::Some(4)	commodity::Freight
eaiquoteid::1741	name::Heren Day Ahead Index (Pence per Therm) ESGM	tenor::Day	bblPerMT::Some(1.0)	limMultiplier::Some(0.01)	businessCalendar::<None>	lotSize::None	indexlevel::Close	volatilityID::None	limSymbol::Some(HERINX.NBP.MONTHLY.CUMULATIVE)	clearPortPrecision::Some(0)	uom::MT	type::PublishedIndex	ccy::GBP	defaultPrecision::Some(5)	commodity::Crude
eaiquoteid::1743	name::Unl 92 Singapore Cargoes vs Prem Unl Euro-Bob OXY NWE Barges (BBL) (Argus)	bblPerMT::Some(8.33)	formula::1.0* MKT(198)- 1.0* MKT(1312) 	volatilityID::None	clearPortPrecision::Some(0)	uom::bbl	type::FormulaIndex	ccy::USD	defaultPrecision::Some(3)
eaiquoteid::1744	name::LLS 1st month vs WTI 1st month (Argus)	bblPerMT::Some(7.57)	formula::1.0* MKT(955)- 1.0* MKT(1096) 	volatilityID::None	clearPortPrecision::Some(2)	uom::bbl	type::FormulaIndex	ccy::USD	defaultPrecision::Some(4)
eaiquoteid::1745	name::WTI Formula Basis month (Argus)	tenor::Day	bblPerMT::Some(7.57)	limMultiplier::Some(1.0)	businessCalendar::ARG	lotSize::None	indexlevel::MidPoint	volatilityID::None	limSymbol::Some(PA0003373.0.1)	clearPortPrecision::Some(0)	uom::bbl	type::PublishedIndex	ccy::USD	defaultPrecision::Some(4)	commodity::Crude
eaiquoteid::1746	name::Unl 95 10ppm CIF NWE basis Thames Cargoes (Platts) Vs Prem Unl Euro-Bob Oxy NWE Barges (Argus)	bblPerMT::Some(8.33)	formula::1.0* MKT(1204)- 1.0* MKT(1312) 	volatilityID::None	clearPortPrecision::Some(0)	uom::MT	type::FormulaIndex	ccy::USD	defaultPrecision::Some(3)
eaiquoteid::1747	name::TC2 UKC to USAC 37KT $/MT (Baltic)	tenor::Day	bblPerMT::None	limMultiplier::Some(1.0)	businessCalendar::IcS	lotSize::None	indexlevel::Val	volatilityID::None	limSymbol::Some(BALTIC.CLEAN.TANKER.ROUTE.TC2.USDMT.CURMON)	clearPortPrecision::Some(0)	uom::MT	type::PublishedIndex	ccy::USD	defaultPrecision::Some(4)	commodity::Freight
eaiquoteid::1748	name::Hard Coking Coal Low Vol FOB Australia (Platts)	tenor::Day	bblPerMT::None	businessCalendar::NYM	lotSize::None	indexlevel::Close	volatilityID::None	clearPortPrecision::Some(0)	uom::MT	type::PublishedIndex	ccy::USD	defaultPrecision::Some(2)	commodity::Coal
eaiquoteid::1749	name::AB-NIT NGX Phys FP (ICE)	tenor::Day	bblPerMT::Some(1.0)	limMultiplier::Some(0.105506)	businessCalendar::PLH	lotSize::None	indexlevel::Index	volatilityID::None	limSymbol::Some(IGBCU21)	clearPortPrecision::Some(0)	uom::MT	type::PublishedIndex	ccy::CAD	defaultPrecision::Some(6)	commodity::NatGas
eaiquoteid::1750	name::Naphtha CIF NWE Cargoes [Low] (Platts)	tenor::Day	bblPerMT::Some(8.9)	limMultiplier::Some(1.0)	businessCalendar::PLE	lotSize::None	indexlevel::Low	volatilityID::None	limSymbol::Some(PAAAL00)	clearPortPrecision::Some(2)	uom::MT	type::PublishedIndex	ccy::USD	defaultPrecision::Some(3)	commodity::Naphtha
eaiquoteid::1751	name::Natural Non-Targa (USD/MT)	bblPerMT::Some(9.0)	formula::1.0* MKT(1753) 	volatilityID::None	clearPortPrecision::Some(4)	uom::MT	type::FormulaIndex	ccy::USD	defaultPrecision::Some(4)
eaiquoteid::1752	name::US Soybean Oil vs CBOT Soybean Oil	tenor::Day	bblPerMT::Some(7.0)	businessCalendar::NYM	lotSize::None	indexlevel::Close	volatilityID::None	clearPortPrecision::Some(0)	uom::lb	type::PublishedIndex	ccy::USD	defaultPrecision::Some(3)	commodity::VegetableOil
eaiquoteid::1753	name::Natural Non-Targa (Conv 398)	tenor::Day	bblPerMT::Some(9.476190476)	limMultiplier::Some(0.01)	businessCalendar::PLH	lotSize::None	indexlevel::Mid	volatilityID::None	limSymbol::Some(PMABY05)	clearPortPrecision::Some(4)	uom::gal	type::PublishedIndex	ccy::USD	defaultPrecision::Some(4)	commodity::Gasoline
eaiquoteid::1754	name::Naphtha FOB Med vs IPE Brent	bblPerMT::Some(8.9)	formula::1.0* MKT(402)- 1.0* MKT(28) 	volatilityID::None	clearPortPrecision::Some(0)	uom::bbl	type::FormulaIndex	ccy::USD	defaultPrecision::Some(3)
eaiquoteid::1755	name::Gas Oil ULSD USGC Pipeline [Low] (Platts)	tenor::Day	bblPerMT::Some(7.44)	limMultiplier::Some(0.01)	businessCalendar::PLH	lotSize::None	indexlevel::Low	volatilityID::None	limSymbol::Some(AATGY00)	clearPortPrecision::Some(0)	uom::gal	type::PublishedIndex	ccy::USD	defaultPrecision::Some(4)	commodity::GasOil
eaiquoteid::1756	name::Unl 85 CBOB Colonial Grade A Pipeline (Argus)	tenor::Day	bblPerMT::Some(8.45)	limMultiplier::Some(0.01)	businessCalendar::ARG	lotSize::None	indexlevel::Mid	volatilityID::None	limSymbol::Some(GP6998A1)	clearPortPrecision::Some(0)	uom::gal	type::PublishedIndex	ccy::USD	defaultPrecision::Some(4)	commodity::Gasoline
eaiquoteid::1757	name::Coal CIF China	tenor::Day	bblPerMT::None	businessCalendar::ARK	lotSize::Some(1000.0)	indexlevel::Close	volatilityID::None	clearPortPrecision::Some(2)	uom::MT	type::PublishedIndex	ccy::USD	defaultPrecision::Some(2)	commodity::Coal
eaiquoteid::1758	name::NYMEX Heat 1st Month vs ICE Brent 1st Month	bblPerMT::Some(7.45)	formula::1.0* MKT(29)- 1.0* MKT(28) 	volatilityID::None	clearPortPrecision::Some(0)	uom::bbl	type::FormulaIndex	ccy::USD	defaultPrecision::Some(4)
eaiquoteid::1759	name::TD7 Cross North Sea 80KT $/MT (Baltic)	tenor::Day	bblPerMT::None	limMultiplier::Some(1.0)	businessCalendar::IcS	lotSize::None	indexlevel::Val	volatilityID::None	limSymbol::Some(BALTIC.DIRTY.TANKER.ROUTE.TD7.USDMT.CURMON)	clearPortPrecision::Some(0)	uom::MT	type::PublishedIndex	ccy::USD	defaultPrecision::Some(4)	commodity::Freight
"""


}
