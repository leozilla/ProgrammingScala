import chapter5._

// val s = Stream.apply(debug(1), debug(2), debug(3), debug(4))
// s.toList
val take2 = Stream(1, 2, 3, 4).foldRight(0)(_ + _)
val x: Stream[Int] = Stream.cons(1, x)
x.takeWhile(_ == 1)
println(x)
println(Stream(1, 2, 3, 4).forAll(x => x > 0))
println(Stream(1, 2, 3, 4).forAll(x => x > 5))
println(Stream(1, 2, 3, 4).forAll(x => x < 2))
val ones: Stream[Int] = Stream.constant(1)
ones.map(_ + 1).exists(_ % 2 == 0)
println(ones.map(_ + 1).take(5).toListRecursive)
println(Stream.from(5).take(15).toListRecursive)
println(Stream.fibs().take(15).toListRecursive)
println(Stream.unfold(10)(s => Some(s, s + 1)).take(5).toListRecursive)
println(Stream.fibsViaUnfold().take(15).toListRecursive)
println(Stream.fromViaUnfold(5).take(15).toListRecursive)
println(Stream.constantViaUnfold(5).take(15).toListRecursive)
println(Stream(1,2,3,4,5).takeViaUnfold(3).toListRecursive)
println(Stream(1,2,3,4,5).takeWhileViaUnfold(n => n < 3).toListRecursive)
println(Stream(1,2,3,4,5).zipAllViaUnfold(Stream(1,2,3,4)).toListRecursive)
print("tails")
println(Stream(1,2,3).tails.toListRecursive)

println(Stream(1,2,3,4,5).hasSubsequence(Stream(2,3)))
