package net.andimiller.soon.models

object Split:
  def apply(divisor: Int)(
      v: Int
  ): (quotient: Int, remainder: Int) = {
    (v / divisor) -> (v % divisor)
  }
