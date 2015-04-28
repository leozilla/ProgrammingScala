package chapter3

import org.scalatest.{Matchers, FlatSpec}

class TreeTest extends FlatSpec with Matchers {

  "Size of tree with only one leaf" should "return one" in {
    val tree = Leaf()

    val actual = Tree.size(tree)

    actual should be (1)
  }

  "Size of tree with two leafs" should "return two" in {
    val tree = Branch(Leaf(0), Leaf(1))

    val actual = Tree.size(tree)

    actual should be (3)
  }

  "Size of tree with two levels" should "return count of leafes" in {
    val tree = Branch(Branch(Leaf(1), Leaf(1)), Branch(Leaf(1), Leaf(1)))

    val actual = Tree.size(tree)

    actual should be (7)
  }

  "Size of tree with three levels" should "return count of leafes" in {
    val tree = Branch(Branch(Leaf(1), Leaf(1)), Branch(Leaf(1), Branch(Leaf(1), Leaf(1))))

    val actual = Tree.size(tree)

    actual should be (9)
  }

  "Depth of tree with only one leaf" should "return one" in {
    val tree = Leaf()

    val actual = Tree.depth(tree)

    actual should be (1)
  }

  "Depth of tree with two leafs" should "return two" in {
    val tree = Branch(Leaf(0), Leaf(1))

    val actual = Tree.depth(tree)

    actual should be (2)
  }

  "Depth of tree with two levels" should "return count of leafes" in {
    val tree = Branch(Branch(Leaf(1), Leaf(1)), Branch(Leaf(1), Leaf(1)))

    val actual = Tree.depth(tree)

    actual should be (3)
  }

  "Depth of tree with three levels" should "return count of leafes" in {
    val tree = Branch(Branch(Leaf(1), Leaf(1)), Branch(Leaf(1), Branch(Leaf(1), Leaf(1))))

    val actual = Tree.depth(tree)

    actual should be (4)
  }

  "DepthViaFold of tree with three levels" should "return count of leafes" in {
    val tree = Branch(Branch(Leaf(1), Leaf(1)), Branch(Leaf(1), Branch(Leaf(1), Leaf(1))))

    val actual = Tree.depthViaFold(tree)

    actual should be (4)
  }
}
