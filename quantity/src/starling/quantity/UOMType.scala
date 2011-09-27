package starling.quantity

import starling.utils.StarlingEnum

case class UOMType(prime: Int)

object UOMType {
  val primesIterator = Primes.primeStreamIterator(3)

  private def create() : UOMType = {
    new UOMType(primesIterator.next)
  }

  val MASS = create()
  val VOLUME = create()
  val OIL_VOLUME = create() // can't convert oil volumes to other volumes: http://en.wikipedia.org/wiki/Oil_barrel#Conversion_to_metric_units
  val TIME = create()
  val HEAT_ENERGY = create()

  /**
   * Returns a unique type. A unit built from this will never be convertible
   * to another unit with separate logic (e.g. Day can't be converted to Month)
   */
  def unique: UOMType = create()

  object Currencies extends StarlingEnum(classOf[UOMType], (d: UOMType) => d.prime.toString) {
    val AED = create() // United Arab Emirates dirham
    val AFN = create() // Afghan afghani
    val ALL = create() // Albanian lek
    val AMD = create() // Armenian dram
    val ANG = create() // Netherlands Antillean guilder
    val AOA = create() // Angolan kwanza
    val ARS = create() // Argentine peso
    val AUD = create() // Australian dollar
    val AWG = create() // Aruban florin
    val AZN = create() // Azerbaijani manat
    val BAM = create() // Bosnia and Herzegovina convertible mark
    val BBD = create() // Barbados dollar
    val BDT = create() // Bangladeshi taka
    val BGN = create() // Bulgarian lev
    val BHD = create() // Bahraini dinar
    val BIF = create() // Burundian franc
    val BMD = create() // Bermudian dollar (customarily known as Bermuda dollar)
    val BND = create() // Brunei dollar
    val BOB = create() // Boliviano
    val BOV = create() // Bolivian Mvdol (funds code)
    val BRL = create() // Brazilian real
    val BSD = create() // Bahamian dollar
    val BTN = create() // Bhutanese ngultrum
    val BWP = create() // Botswana pula
    val BYR = create() // Belarusian ruble
    val BZD = create() // Belize dollar
    val CAD = create() // Canadian dollar
    val CDF = create() // Congolese franc
    val CHE = create() // WIR Euro (complementary currency)
    val CHF = create() // Swiss franc
    val CHW = create() // WIR Franc (complementary currency)
    val CLF = create() // Unidad de Fomento (funds code)
    val CLP = create() // Chilean peso
    val CNY = create() // Chinese yuan
    val COP = create() // Colombian peso
    val COU = create() // Unidad de Valor Real
    val CRC = create() // Costa Rican colon
    val CUC = create() // Cuban convertible peso
    val CUP = create() // Cuban peso
    val CVE = create() // Cape Verde escudo
    val CZK = create() // Czech koruna
    val DJF = create() // Djiboutian franc
    val DKK = create() // Danish krone
    val DOP = create() // Dominican peso
    val DZD = create() // Algerian dinar
    val EGP = create() // Egyptian pound
    val ERN = create() // Eritrean nakfa
    val ETB = create() // Ethiopian birr
    val EUR = create() // Euro
    val FJD = create() // Fiji dollar
    val FKP = create() // Falkland Islands pound
    val GBP = create() // Pound sterling
    val GEL = create() // Georgian lari
    val GHS = create() // Ghanaian cedi
    val GIP = create() // Gibraltar pound
    val GMD = create() // Gambian dalasi
    val GNF = create() // Guinean franc
    val GTQ = create() // Guatemalan quetzal
    val GYD = create() // Guyanese dollar
    val HKD = create() // Hong Kong dollar
    val HNL = create() // Honduran lempira
    val HRK = create() // Croatian kuna
    val HTG = create() // Haitian gourde
    val HUF = create() // Hungarian forint
    val IDR = create() // Indonesian rupiah
    val ILS = create() // Israeli new sheqel
    val INR = create() // Indian rupee
    val IQD = create() // Iraqi dinar
    val IRR = create() // Iranian rial
    val ISK = create() // Icelandic króna
    val JMD = create() // Jamaican dollar
    val JOD = create() // Jordanian dinar
    val JPY = create() // Japanese yen
    val KES = create() // Kenyan shilling
    val KGS = create() // Kyrgyzstani som
    val KHR = create() // Cambodian riel
    val KMF = create() // Comoro franc
    val KPW = create() // North Korean won
    val KRW = create() // South Korean won
    val KWD = create() // Kuwaiti dinar
    val KYD = create() // Cayman Islands dollar
    val KZT = create() // Kazakhstani tenge
    val LAK = create() // Lao kip
    val LBP = create() // Lebanese pound
    val LKR = create() // Sri Lanka rupee
    val LRD = create() // Liberian dollar
    val LSL = create() // Lesotho loti
    val LTL = create() // Lithuanian litas
    val LVL = create() // Latvian lats
    val LYD = create() // Libyan dinar
    val MAD = create() // Moroccan dirham
    val MDL = create() // Moldovan leu
    val MGA = create() // Malagasy ariary
    val MKD = create() // Macedonian denar
    val MMK = create() // Myanma kyat
    val MNT = create() // Mongolian tugrik
    val MOP = create() // Macanese pataca
    val MRO = create() // Mauritanian ouguiya
    val MUR = create() // Mauritian rupee
    val MVR = create() // Maldivian rufiyaa
    val MWK = create() // Malawian kwacha
    val MXN = create() // Mexican peso
    val MXV = create() // Mexican Unidad de Inversion (UDI) (funds code)
    val MYR = create() // Malaysian ringgit
    val MZN = create() // Mozambican metical
    val NAD = create() // Namibian dollar
    val NGN = create() // Nigerian naira
    val NIO = create() // Cordoba oro
    val NOK = create() // Norwegian krone
    val NPR = create() // Nepalese rupee
    val NZD = create() // New Zealand dollar
    val OMR = create() // Omani rial
    val PAB = create() // Panamanian balboa
    val PEN = create() // Peruvian nuevo sol
    val PGK = create() // Papua New Guinean kina
    val PHP = create() // Philippine peso
    val PKR = create() // Pakistani rupee
    val PLN = create() // Polish zloty
    val PYG = create() // Paraguayan guaraní
    val QAR = create() // Qatari rial
    val RON = create() // Romanian new leu
    val RSD = create() // Serbian dinar
    val RUB = create() // Russian rouble
    val RWF = create() // Rwandan franc
    val SAR = create() // Saudi riyal
    val SBD = create() // Solomon Islands dollar
    val SCR = create() // Seychelles rupee
    val SDG = create() // Sudanese pound
    val SEK = create() // Swedish krona/kronor
    val SGD = create() // Singapore dollar
    val SHP = create() // Saint Helena pound
    val SLL = create() // Sierra Leonean leone
    val SOS = create() // Somali shilling
    val SRD = create() // Surinamese dollar
    val SSP = create() // South Sudanese pound
    val STD = create() // São Tomé and Príncipe dobra
    val SYP = create() // Syrian pound
    val SZL = create() // Lilangeni
    val THB = create() // Thai baht
    val TJS = create() // Tajikistani somoni
    val TMT = create() // Turkmenistani manat
    val TND = create() // Tunisian dinar
    val TOP = create() // Tongan pa?anga
    val TRY = create() // Turkish lira
    val TTD = create() // Trinidad and Tobago dollar
    val TWD = create() // New Taiwan dollar
    val TZS = create() // Tanzanian shilling
    val UAH = create() // Ukrainian hryvnia
    val UGX = create() // Ugandan shilling
    val USD = create() // United States dollar
    val UYI = create() // Uruguay Peso en Unidades Indexadas (URUIURUI) (funds code)
    val UYU = create() // Uruguayan peso
    val UZS = create() // Uzbekistan som
    val VEF = create() // Venezuelan bolívar fuerte
    val VND = create() // Vietnamese d?ng
    val VUV = create() // Vanuatu vatu
    val WST = create() // Samoan tala
    val XAF = create() // CFA franc BEAC
    val XAG = create() // Silver (one troy ounce)
    val XAU = create() // Gold (one troy ounce)
    val XBA = create() // European Composite Unit(EURCO) (bond market unit)
    val XBB = create() // European Monetary Unit (E.M.U.-6) (bond market unit)
    val XBC = create() // European Unit of Account 9(E.U.A.-9) (bond market unit)
    val XBD = create() // European Unit of Account 17(E.U.A.-17) (bond market unit)
    val XCD = create() // East Caribbean dollar
    val XDR = create() // Special Drawing Rights
    val XFU = create() // UIC franc (special settlement currency)
    val XOF = create() // CFA Franc BCEAO
    val XPD = create() // Palladium (one troy ounce)
    val XPF = create() // CFP franc
    val XPT = create() // Platinum (one troy ounce)
    val XTS = create() // Code reserved for testing purposes
    val XXX = create() // No currency
    val YER = create() // Yemeni rial
    val ZAR = create() // South African rand
    val ZMK = create() // Zambian kwacha
    val ZWL = create() // Zimbabwe dollar

    val WSC = create() // World scale - not really a currency

    def isCurrency(r: Ratio) = r.denominator == 1 && find(r.numerator.toString).isDefined
  }
}