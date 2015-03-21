package chapter3

import org.scalatest._

class ListTest extends FlatSpec with Matchers {

  "Calling setHead on list with one element" should "replace this one element" in {
    val single = List(1)

    val result = List.setHead(single, 2)

    List.head(result) should be (2)
  }

  "Calling setHead on an empty list" should "generate an MatchError" in {
    val emptyList = Nil
    a [MatchError] should be thrownBy {
      List.setHead(emptyList, 1)
    }
  }

  "Dropping one element from list with at least one element" should "return a new list without the first element" in {
    val moreThanOne = List(1,2,3)

    val result = List.drop(moreThanOne, 1)
    
    List.head(result) should be (2)
    List.tail(result) should be (List(3))
  }

  "Dropping two elements from list with at least two elements" should "return a new list without the first two elements" in {
    val atLeastTwo = List(1,2,3)

    val result = List.drop(atLeastTwo, 2)

    List.head(result) should be (3)
    List.tail(result) should be (Nil)
    List.size(result) should be (1)
  }
}
