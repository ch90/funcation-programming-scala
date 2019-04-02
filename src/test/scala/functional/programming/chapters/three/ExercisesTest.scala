package functional.programming.chapters.three

import org.scalatest.FunSuite


class ExercisesTest extends FunSuite {


  //3_2
  test("Exercises.tail") {
    assert(Exercises.tail(List(1, 2, 3, 4, 5, 6)) === List(2, 3, 4, 5, 6))
    assert(Exercises.tail(List(1)) === Nil)
    assert(Exercises.tail(Nil) === Nil)
  }


  //3_3
  test("Exercises.setHead") {
    assert(Exercises.setHead(List(1, 2, 3, 4, 5, 6), 7) === List(7, 2, 3, 4, 5, 6))
    assert(Exercises.setHead(List(1), 2) === List(2))
  }


  //3_4
  test("Exercises.drop") {
    assert(Exercises.drop(List(1, 2, 3, 4, 5, 6), 3) === List(4, 5, 6))
    assert(Exercises.drop(List(1, 2, 3, 4, 5, 6), 2) === List(3, 4, 5, 6))
    assert(Exercises.drop(List(1), 2) === Nil)
    assert(Exercises.drop[Int](Nil, 1) === Nil)
  }


  //3_5
  test("Exercises.dropWhile") {
    def isEven(x: Int): Boolean = {
      x % 2 == 0
    }

    assert(Exercises.dropWhile(List(1, 2, 3, 4, 5, 6), isEven) === List(2,4,6))
    assert(Exercises.dropWhile(List(2,4), isEven) === List(2,4))
    assert(Exercises.dropWhile(List(2,3,4), isEven) === List(2,4))
    assert(Exercises.dropWhile(List(1), isEven) === Nil)
    assert(Exercises.dropWhile[Int](Nil, isEven) === Nil)
  }


  //3_6
  test("Exercises.init") {
    assert(Exercises.init(List(1, 2, 3, 4, 5, 6)) === List(1, 2, 3, 4, 5))
        assert(Exercises.init(List(1)) === Nil)
        assert(Exercises.init(Nil) === Nil)


  }


  //3_9
  test("Exercises.foldRight") {
    assert(Exercises.foldRight(List(1, 2, 3, 4, 5, 6), 0)((_, y) => y + 1) === 6)
    assert(Exercises.foldRight(List(1), 0)((_, y) => y + 1) === 1)
    assert(Exercises.foldRight(Nil, 0)((_, y) => y + 1) === 0)
  }


  //3_10
  test("Exercises.foldLeft") {
    assert(Exercises.foldLeft(List(1, 2, 3, 4, 5, 6), 0)((y, _) => y + 1) === 6)
    assert(Exercises.foldLeft(List(1), 0)((y, _) => y + 1) === 1)
    assert(Exercises.foldLeft(Nil, 0)((y, _) => y + 1) === 0)
  }


  //3.11
  test("Exercises.sumProd") {
    assert(Exercises.foldLeft(List(1, 2, 3, 4, 5, 6), 0)(_ + _) === 21)
    assert(Exercises.foldLeft(List(1, 2, 3, 4, 5, 6), 1)(_ * _) === 720)
  }


  //3.12
  test("Exercises.reverse") {
    def f[Int](x: List[Int], y: Int): List[Int] =
      ::(y, x)
    assert(Exercises.foldLeft(List(1, 2, 3), List[Int]())(f) === List(3, 2, 1))
  }


  //3.14
  test("Exercises.append") {
    def f[Int](x: List[Int], y: Int): List[Int] =
      ::(y, x)
    assert(Exercises.foldLeft(List(4,5,6),List(1, 2, 3))(f) === List(6, 5, 4, 1, 2, 3))
  }

  //3.15
  test("Exercises.flatMap") {
    def g[Int](x: List[Int], y: Int): List[Int] =
      ::(y, x)

    def f[Int](x: List[Int], y: List[Int]): List[Int] =
        x.foldLeft(y)(g)


    assert(Exercises.foldLeft(List(List(1, 2), List(3, 4), List(5,6)), List[Int]())(f) === List(4, 3, 1, 2, 5, 6))
  }

  //3.16
  test("Exercises.addOne") {
    assert(Exercises.addOne(List(4,5,6)) === List(5,6,7))
  }


  //3.25
  test("Exercises.trees") {
    val leafA = new Exercises.Leaf("A")
    val leafB = new Exercises.Leaf("B")
    val leafC = new Exercises.Leaf("C")
    val leafD = new Exercises.Leaf("D")
    val leafE = new Exercises.Leaf("E")
    val leafF = new Exercises.Leaf("F")
    val branch1 = new Exercises.Branch(leafA,leafB)
    val branch2 = new Exercises.Branch(leafC,leafD)
    val branch3 = new Exercises.Branch(leafE,leafF)
    val branch4 = new Exercises.Branch(branch3,branch2)
    val tree = new Exercises.Branch(branch4,branch1)

    assert(Exercises.size(tree)===11)
  }

  //3.26
  test("Exercises.maxtrees") {
    assert(Exercises.maximum(createTree(1,15))===15)
    assert(Exercises.maximum(createTree(15,1))===15)
  }


  def createTree(x:Int, y: Int): Exercises.Tree[Int] = {
    val leafA = new Exercises.Leaf(y)
    val leafB = new Exercises.Leaf(2)
    val leafC = new Exercises.Leaf(3)
    val leafD = new Exercises.Leaf(x)
    val leafE = new Exercises.Leaf(4)
    val leafF = new Exercises.Leaf(5)
    val branch1 = new Exercises.Branch(leafA,leafB)
    val branch2 = new Exercises.Branch(leafC,leafD)
    val branch3 = new Exercises.Branch(leafE,leafF)
    val branch4 = new Exercises.Branch(branch3,branch2)
    new Exercises.Branch(branch4,branch1)
  }


  //3.27
  test("Exercises.depth") {
    assert(Exercises.depth(createTree(1,15))===4)
    val tree = Exercises.Branch(createTree(0,0),new Exercises.Leaf(2))
    assert(Exercises.depth(tree)===5)
  }




}