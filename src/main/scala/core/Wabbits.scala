package core

class Wabbits {
  def litter(months: BigInt, litterSize: BigInt): BigInt = {
    def lit(count: BigInt): BigInt = {
      if (count <= 2) 1
      else lit(count - 1) + litterSize * lit(count - 2)
    }

    lit(months)

    // "1234567"
  }
}
