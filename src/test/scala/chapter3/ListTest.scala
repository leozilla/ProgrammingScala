package chapter3

import chapter3.List
import org.scalatest._

class ListTest extends FlatSpec with Matchers {

  /*
  "Calling setHead on list with one element" should "new list with element" in {
    val single = List(1)

    val actual = List.setHead(single, 2)

    actual should be (2)
  }

  "Calling setHead2 on list with one element" should "new list with element" in {
    val single = List(1)

    val actual = List.setHead2(single, 2)

    actual should be (2)
  }

  "Calling setHead2 on list with multiple element" should "new list with element" in {
    val in = List(1,2,3)

    val actual = List.setHead2(in, 9)

    actual should be (List(9,2,3))
  }
  */

  "Calling setHead on an empty list" should "generate an MatchError" in {
    val emptyList = Nil
    a [MatchError] should be thrownBy {
      List.setHead(emptyList, 1)
    }
  }

  "Tail of empty list" should "return empty list" in {
    val in = List()

    val actual = List.tail(in)

    actual should be (Nil)
  }

  "Tail of list with one element" should "return empty list" in {
    val in = List(1)

    val actual = List.tail(in)

    actual should be (Nil)
  }

  "Tail of list with at least one element" should "return list without head" in {
    val in = List(1,2,3,4)

    val actual = List.tail(in)

    actual should be (List(2,3,4))
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

    val actual = List.dropWhile(list)(e => e != 3)

    actual should be (List(3,4,5))
  }

  "Dropping elements while element equal some value and element not contained" should "return Nil" in {
    val list = List(1,2,3)

    val actual = List.dropWhile(list)(e => e != 99)

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

    val actual = List.sumLeft(list)

    actual should be (10)
  }

  "Product using foldLeft" should "return multiplication of all elements" in {
    val list = List(1d,2d,3d,4d)

    val actual = List.productLeft(list)

    actual should be (24)
  }

  "Length using foldLeft" should "return zero when list is empty" in {
    val list = List()

    val actual = List.lengthLeft(list)

    actual should be (0)
  }

  "Length using foldLeft" should "return count of all elements" in {
    val list = List(1d,2d,3d,4d)

    val actual = List.lengthLeft(list)

    actual should be (4)
  }

  "Reverse a list with at least on element" should "return the list in reversed order" in {
    val list = List(1,2,3)

    val actual = List.reverse(list)

    actual should be (List(3,2,1))
  }

  "Append two empty lists" should "return an empty list" in {
    val list1 = List()
    val list2 = List()

    val actual = List.append(list1, list2)

    actual should be (List())
  }

  "Append two lists with only one element" should "return a list with element of first list before element of second list" in {
    val list1 = List(1)
    val list2 = List(2)

    val actual = List.append(list1, list2)

    actual should be (List(1,2))
  }

  "Append an empty list after a list with one element" should "return list with one element" in {
    val list1 = List(1)
    val list2 = List()

    val actual = List.append(list1, list2)

    actual should be (List(1))
  }

  "Append a list with at least on element after an empty list" should "return only the list wich has elements" in {
    val list1 = List()
    val list2 = List(2,3)

    val actual = List.append(list1, list2)

    actual should be (List(2,3))
  }

  "Append two lists with at least one element" should "return the concatenated version of both lists" in {
    val list1 = List(1,2,3)
    val list2 = List(4,5,6)

    val actual = List.append(list1, list2)

    actual should be (List(1,2,3,4,5,6))
  }

  "Append one to each element of a list" should "return new list with one aded to each element" in {
    val list = List(1,2,3)

    val actual = List.map(list)(x => x + 1)

    list should be (List(1,2,3))
    actual should be (List(2,3,4))
  }

  "Convert list of Double to String" should "return new list of Double converted to Strings" in {
    val list = List(1d,2d,3d)

    val actual = List.map(list)(x => x.toString)

    list should be (List(1d,2d,3d))
    actual should be (List("1.0","2.0","3.0"))
  }

  "Filter all odd numbers from a given list" should "return new list without the odd numbers" in {
    val list = List(1,2,3,4,5)

    val actual = List.filter(list)(x => (x % 2) == 0)

    list should be (List(1,2,3,4,5))
    actual should be (List(2,4))
  }

  "Flatmap" should "return new list with flat mapped result" in {
    val list = List(1,2,3)

    val actual = List.flatMap(List(1,2,3))(i => List(i,i))

    list should be (List(1,2,3))
    actual should be (List(1,1,2,2,3,3))
  }

  /*
  "Zip two empty lists" should "return Nil list" in {
    val list1 = List()
    val list2 = List()

    val actual = List.zipWith(list1, list2)((a: Int, b: Int) => a + b)

    list1 should be (Nil)
    list2 should be (Nil)
    actual should be (Nil)
  }

  "Zip two lists with one element" should "return new zipped list" in {
    val list1 = List(1)
    val list2 = List(3)

    val actual = List.zipWith(list1, list2)(_ + _)

    list1 should be (List(1))
    list2 should be (List(3))
    actual should be (List(4))
  }

  "Zip two lists with at least one element" should "return new zipped list" in {
    val list1 = List(1,2)
    val list2 = List(3,4)

    val actual = List.zipWith(list1, list2)(_ + _)

    list1 should be (List(1,2))
    list2 should be (List(3,4))
    actual should be (List(4,6))
  }
  */

  "Fold" should "return the concatenated version of both lists" in {
    val list1 = List(1,2,3)

    val actual = List.foldRightViaFoldLeft(list1, Nil:List[Int])((x, xs) => {
      println("a = " + x + "\tsubtotal = " + xs)
      Cons(x, xs)
    })

    actual should be (List(2,3,4))
  }
}
