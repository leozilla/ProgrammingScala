package chapter3

import org.scalatest._

class ListTest extends FlatSpec with Matchers {

  "Calling setHead on list with one element" should "replace this one element" in {
    val single = List(1)

    val actual = List.setHead(single, 2)

    List.head(actual) should be (2)
  }

  "Calling setHead on an empty list" should "generate an MatchError" in {
    val emptyList = Nil
    a [MatchError] should be thrownBy {
      List.setHead(emptyList, 1)
    }
  }

  "Dropping one element from list with at least one element" should "return a new list without the first element" in {
    val moreThanOne = List(1,2,3)

    val actual = List.drop(moreThanOne, 1)

    actual should be (List(2, 3))
  }

  "Dropping two elements from list with at least two elements" should "return a new list without the first two elements" in {
    val atLeastTwo = List(1,2,3)

    val actual = List.drop(atLeastTwo, 2)

    actual should be (List(3))
    List.size(actual) should be (1)
  }

  "Dropping elements while element equal some value and element contained" should "return tail of list after element was found" in {
    val list = List(1,2,3,4,5)

    val actual = List.dropWhile(list, (e: Int) => e != 3)

    actual should be (List(4, 5))
  }

  "Dropping elements while element equal some value and element not contained" should "return Nil" in {
    val list = List(1,2,3)

    val actual = List.dropWhile(list, (e: Int) => e != 99)

    actual should be (Nil)
  }

  "Init an empty list" should "return an empty list" in {
    val actual = List.init(Nil)

    actual should be (Nil)
  }

  "Init a list of only one element" should "return an empty list" in {
    val list = List(1)

    val actual = List.init(list)

    actual should be (Nil)
  }

  "Init a list of two element" should "return only head of list" in {
    val list = List(1,2)

    val actual = List.init(list)

    actual should be (List(1))
  }

  "Init a list of three elements" should "return new list with only the first two elements" in {
    val list = List(1,2,3)

    val actual = List.init(list)

    actual should be (List(1, 2))
  }

  "Sum" should "return sum of all elements" in {
    val list = List(1,2,3,4)

    val actual = List.sum(list)

    actual should be (10)
  }

  "Product" should "return product of all elements" in {
    val list = List(1d,2d,3d,4d)

    val actual = List.product(list)

    actual should be (24)
  }

  "Product2" should "return 0.0 if any element is 0.0" in {
    val list = List(1d,0.0d,3d,4d)

    val actual = List.product2(list)

    actual should be (0.0)
  }

  "Product2" should "return product of all elements" in {
    val list = List(1d,2d,3d,4d)

    val actual = List.product2(list)

    actual should be (24)
  }

  "Exercise 3.8" should "i dont now" in {
    val actual = List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))

    actual should be (List(1,2,3))
  }

  "Length of empty list" should "return 0" in {
    val list = Nil

    val actual = List.length(list)

    actual should be (0)
  }

  "Length of none empty list" should "return return number of elements" in {
    val list = List(1d,2d,3d,4d)

    val actual = List.length(list)

    actual should be (4)
  }

  "Sum using foldLeft" should "return sum of all elements" in {
    val list = List(1d,2d,3d,4d)

    val actual = List.foldLeft(list, 0d)(_ + _)

    actual should be (10)
  }

  "Product using foldLeft" should "return multiplication of all elements" in {
    val list = List(1d,2d,3d,4d)

    val actual = List.foldLeft(list, 1.0d)(_ * _)

    actual should be (24)
  }

  "Length using foldLeft" should "return zero when list is empty" in {
    val list = List()

    val actual = List.foldLeft(list, 0)((a, _) => a + 1)

    actual should be (0)
  }

  "Length using foldLeft" should "return count of all elements" in {
    val list = List(1d,2d,3d,4d)

    val actual = List.foldLeft(list, 0)((a, _) => a + 1)

    actual should be (4)
  }
}
