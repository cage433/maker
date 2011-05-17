package starling.utils


trait CompLong {
  implicit def LongToCompInt(l:Long) = new CompLong(l)

  class CompLong(l:Long) {
    def ks = l / 1024
    def megs = ks / 1024
    def gigs = megs / 1024
  }
}
