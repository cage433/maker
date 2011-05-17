package starling.utils


trait CompInt {
  implicit def IntToCompInt(i:Int) = new CompInt(i)

  class CompInt(i:Int) {
    def ks = i / 1024
    def megs = ks / 1024
    def gigs = megs / 1024
  }
}
