import chapter6.SimpleRNG

val rng = SimpleRNG(System.nanoTime())
val valueAndState = rng.nextInt
println(valueAndState._1)