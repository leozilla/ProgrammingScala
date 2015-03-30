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

  "Init a list of three elements" should "return new list with only the first two elements" in {
    val list = List(1,2,3)

    val actual = List.init(list)

    actual should be (List(1, 2))
  }
}
