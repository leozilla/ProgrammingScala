import chapter6.{RNG, SimpleRNG}

val rng = SimpleRNG(System.nanoTime())
val (num1, state1) = rng.nextInt
println(num1)
val (num2, state2) = RNG.nonNegativeInt(state1)
println(num2)
val (num3, state3) = RNG.nonNegativeInt(state2)
println(num3)
val (num4, state4) = RNG.double(state3)
println(num3)
println(RNG.ints(3)(state4))