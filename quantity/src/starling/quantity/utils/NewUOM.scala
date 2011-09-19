package starling.quantity.utils

import math.pow

object NewUOM {

  object Ratio {
    def gcd(a: Long, b: Long): Long = b match {
      case 0 => a
      case _ => gcd(b, a % b)
    }
  }

  case class Ratio(numerator: Long, denominator: Long) {
    def gcd = if (numerator == 0)
      1l
    else {
      Ratio.gcd(numerator, denominator)
    }

    def reduce = if (numerator == 0)
      this
    else {
      val g = Ratio.gcd(numerator, denominator)
      Ratio(numerator / g, denominator / g)
    }

    def *(rhs: Long) = Ratio(numerator * rhs, denominator)

    def *(rhs: Ratio) = Ratio(numerator * rhs.numerator, denominator * rhs.denominator)

    def /(rhs: Ratio) = this * (rhs.inverse)

    def ^(power: Int) = {
      def positivePower(u: Ratio, p: Int) = Ratio(pow(u.numerator, p).asInstanceOf[Long], pow(u.denominator, p).asInstanceOf[Long])

      if (power >= 0) {
        positivePower(this, power)
      } else {
        positivePower(inverse, -power)
      }
    }

    def inverse = Ratio(denominator, numerator)

    def doubleValue = denominator.doubleValue / numerator.doubleValue

    def gcd(rhs: Ratio) = Ratio(Ratio.gcd(numerator, rhs.numerator), Ratio.gcd(denominator, rhs.denominator))

    def numeratorRatio = Ratio(numerator, 1)

    def denominatorRatio = Ratio(denominator, 1)
  }

  case class Symbol(prime: Long, names: String*) {
    def name = names.head
  }

  val CENT_SYM = Symbol(3, "C")
  val USD_SYM = Symbol(5, "USD")
  val G_SYM = Symbol(7, "G")
  val MT_SYM = Symbol(11, "MT")

  val symbols = List(CENT_SYM, USD_SYM, G_SYM, MT_SYM)
  val primes: List[Long] = List(3, 5, 7, 11, 13, 17)
  val primeToSymbol = symbols.map(s => s.prime -> s).toMap


  val USD = Ratio(13, 1)
  val MASS = Ratio(17, 1)

  val cent = new UOM(USD, Ratio(CENT_SYM.prime, 1), 1)
  val usd = new UOM(USD, Ratio(USD_SYM.prime, 1), 100)
  val g = new UOM(MASS, Ratio(G_SYM.prime, 1), 1)
  val mt = new UOM(MASS, Ratio(MT_SYM.prime, 1), 1e6)

  val uoms = List(cent, usd, g, mt)
  val uomMap = uoms.map(u => (u.uType.numerator, u.subType.numerator) -> u).toMap

  case class UOM(uType: Ratio, subType: Ratio, v: BigDecimal) {
    def plus(o: UOM): Option[BigDecimal] = o match {
      case UOM(`uType`, `subType`, _) => Some(1.0)
      case UOM(`uType`, oSubType, oV) => Some(oV / v)
      case _ => None
    }

    def minus(o: UOM): Option[BigDecimal] = this.plus(o)

    def /(o: UOM) = this * o.inverse

    def *(o: UOM): UOM = {
      val notReduced = UOM(uType * o.uType, subType * o.subType, v * o.v)
      val uGCD = notReduced.uType.gcd
      if (uGCD > 1) {
        val sGCD = notReduced.subType.gcd
        val decomposedU = decomposePrimes(uGCD)
        val decomposedS = decomposePrimes(sGCD)
        if (decomposedU.values.sum == decomposedS.values.sum) {
          val reduced = notReduced.copy(uType = notReduced.uType.reduce, subType = notReduced.subType.reduce)

          reduced
        } else {
          def remove(pp: Long, primes: List[Long], removed: Int, max: Int): Long = if (removed < max) {
            val prime = primes.find(p => pp % p == 0).get
            val newPP = pp / prime
            remove(newPP, primes, removed + 1, max)
          } else {
            pp
          }
          var subType = notReduced.subType
          decomposedU.map {
            case (u, num) => {
              val matches = uomMap.keySet.filter {
                case (a, _) => a == u
              }.map(_._2).toList
              subType = Ratio(remove(subType.numerator, matches, 0, num), remove(subType.denominator, matches, 0, num))
            }
          }

          val uType = notReduced.uType.reduce
          val bla = notReduced.copy(uType, subType)
          bla
        }
      } else {
        notReduced
      }
    }

    def inverse = UOM(uType.inverse, subType.inverse, 1 / v)

    override def toString = asString

    lazy private val asString = {
      val symMap = asSymbolMap.map {
        case (sym, power) => (sym.name, power)
      }.toList.sortWith(_._1 < _._1)

      val num = symMap.map {
        case (symName, power) if power == 1 => symName
        case (symName, power) if power > 1 => symName + "^" + power
        case _ => ""
      }.foldLeft("")(_ + _)

      val den = symMap.map {
        case (symName, power) if power == -1 => symName
        case (symName, power) if power < -1 => symName + "^" + -power
        case _ => ""
      }.foldLeft("")(_ + _)

      val sep = "/"

      (num, den) match {
        case (num, "") => num
        case ("", den) => den
        case _ => num + sep + den
      }
    }

    def decomposePrimes(n: Long): Map[Long, Int] = {
      def recurse(n: Long, primes: List[Long], acc: Map[Long, Int]): Map[Long, Int] = n match {
        case 0 => acc // Should only happen for the null unit
        case 1 => acc
        case _ => primes match {
          case p :: rest =>
            if (n % p == 0)
              recurse(n / p, primes, acc + (p -> (acc.getOrElse(p, 0) + 1)))
            else
              recurse(n, rest, acc)
          case Nil =>
            throw new IllegalStateException("Prime decomposition is badly wrong")
        }
      }
      recurse(n, primes, Map.empty[Long, Int])
    }

    def asSymbolMap: Map[Symbol, Int] = {
      // Note - both NULL and SCALAR become an empty map

      val reducedUOM = this
      val negativePowers = decomposePrimes(reducedUOM.subType.denominator).mapValues(_ * -1)
      val primePowers = decomposePrimes(reducedUOM.subType.numerator) ++ negativePowers
      Map.empty ++ primePowers.map {
        case (p, n) => (primeToSymbol(p) -> n)
      }
    }
  }

  def main(args: Array[String]) {
    locally {
      val aa = usd / g
      val bb = cent / g

      val bla = aa / bb
      println("bla:: " + bla + ", " + bla.v)
      println("bla:: " + bla.asSymbolMap)
    }
    locally {
      val aa = usd / g
      val bb = cent / g

      val bla = aa.plus(bb)
      println("bla1:: " + bla)
      println("bla1:: " + (1 + bla.get * 50) + " " + aa)
    }
    locally {
      val aa = cent * usd
      val bb = mt * cent
      val bla = aa / bb
      println("bla:: " + bla)
      println("bla:: " + bla.asSymbolMap)
      assert(bla.toString == "USD/MT")
    }
    locally {
      val aa = cent * usd * usd
      val bb = mt * cent * usd
      val bla = aa / bb
      println("bla:: " + bla)
      println("bla:: " + bla.asSymbolMap)
      assert(bla.toString == "USD/MT")
    }
    locally {
      val aa = cent * usd * cent
      val bb = mt * cent * usd
      val bla = aa / bb
      println("bla:: " + bla)
      println("bla:: " + bla.asSymbolMap)
      assert(bla.toString == "C/MT")
    }
    locally {
      val aa = usd * cent * usd
      val bb = mt * usd * usd
      val bla = aa / bb
      println("bla:: " + bla)
      println("bla:: " + bla.asSymbolMap)
      assert(bla.toString == "C/MT")
    }
    locally {
      val aa = cent * usd * cent
      val bb = mt * usd * usd
      val bla = aa / bb
      println("bla:: " + bla)
      println("bla:: " + bla.asSymbolMap)
      assert(bla.toString == "USD/MT")
    }

    locally {
      val aa = usd * usd
      val bb = mt * cent
      val bla = aa / bb

      println("bla:: " + bla)
      println("bla:: " + bla.asSymbolMap)
      assert(bla.toString == "USD/MT")
    }

    val usdPerG = usd / g
    println("usdPerG:: " + usdPerG + ", " + usdPerG.asSymbolMap)
    val usdPerMT = usd / mt
    println("usdPerMT:: " + usdPerMT + ", " + usdPerMT.asSymbolMap)

    //1000 usd per mt + .1usd per g = $101000/mt
    val added = usdPerMT.plus(usdPerG) match {
      case Some(mult) => 1000 + mult.toDouble * .1
    }
    println("added:: " + added + " " + usdPerMT)
    assert(added == 101000)

    val comp = usdPerMT / usdPerG
    println("comp:: " + comp + " -- " + comp.v)
    println("comp.inv:: " + comp.inverse + " -- " + comp.inverse.v)
  }
}