package starling.calendar


import starling.daterange.{Year, Day}

object ReligiousCalendar {
	/**
	 * Calculates the day that Easter Sunday will fall on, according to the Catholic/Protestant convention.
	 *
	 * Calculates Easter (Sunday) every year in the range 1900-2099. If you're still using this close to
	 * 2099 then go find out about the "Nature 1876" algorithm. This algorithm is called Carter's algorithm
	 * and I stole it from http://www.smart.net/~mmontes/carter.html
	 *
	 * NOTE: This is the Catholic/Protestant definition of Easter, not the Orthodox one...
	 */
	def catholicEaster(year : Int) : Day = {
		// well-named temporaries from the FORTRAN school...
		var b, d, e, q : Int = 0
		val y = year

		b = 255 - 11 * (y % 19);
		d = ((b - 21) % 30) + 21;
		if (d > 48) d -= 1;
		e = (y + (y / 4) + d + 1) % 7;
		q = d + 7 - e;

		if (q < 32) {
			Day(y, 3, q);
		} else {
			Day(y, 4, q - 31);
		}
	}

	/**
	 * Calculates the date of Pentecost, or Whit Sunday.
	 *
	 * This is (apparently) the seventh week after Easter, see: http://en.wikipedia.org/wiki/Pentecost
	 */
	def pentecost(year : Int) : Day = {
		return catholicEaster(year) + 49
	}

}
