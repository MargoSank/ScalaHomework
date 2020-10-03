// Homework. Implement functions that calculate https://en.wikipedia.org/wiki/Lowest_common_denominator and
// https://en.wikipedia.org/wiki/Greatest_common_divisor for integers.
object Basics {
  def main(args: Array[String]): Unit = {

    def gcd(a: Int, b: Int): Int = {
      val hi = Math.max(a, b)
      val lo = Math.min(a, b)

      if (hi % lo == 0) lo
      else gcd(lo, hi % lo)
    }

    def lcd(a: Int, b: Int): Int = Math.abs(a * b) / gcd(a, b)


    println(gcd(16, 8)) //4
    println(lcd(16, 8)) //16

    println(gcd(16, 20)) //4
    println(lcd(16, 20)) //80
  }
}
