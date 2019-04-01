package functional.programming.chapters.three

object Exercises {


  def main(args: Array[String]): Unit = {
    //    exercise3_1()
    //    exercise3_2()
    //    exercise3_3()
    //    exercise3_4()
//    exercise3_5()
    exercise3_6()
  }


  def exercise3_1(): Unit = {
    val x = List(1, 2, 3, 4, 5) match {
      case ::(x, ::(2, ::(4, _))) => x
      case Nil => 42
      case ::(x, ::(y, ::(3, ::(4, _)))) => x + y
      case ::(h, t) => h
      case _ => 101
    }
    println(x)
  }

  def exercise3_2(): Unit = {
    println(tail(List(1, 2, 3, 4, 5, 6)))
    println(tail(List(1)))
    println(tail(Nil))
  }


  def tail[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case ::(_, a) => a
  }

  def exercise3_3(): Unit = {
    println(setHead(List(1, 2, 3, 4, 5, 6), 7))
    println(setHead(List(1), 2))
    println(setHead[Int](Nil, 1))
  }


  def setHead[A](list: List[A], item: A): List[A] = list match {
    case Nil => Nil
    case ::(_, a) => item :: a
  }


  def exercise3_4(): Unit = {
    println(drop(List(1, 2, 3, 4, 5, 6), 3))
    println(drop(List(1, 2, 3, 4, 5, 6), 2))
    println(drop(List(1), 2))
    println(drop[Int](Nil, 1))
  }

  def drop[A](l: List[A], n: Int): List[A] = {

    def loop(l: List[A], n: Int): List[A] = {
      if (n == 0) l
      else if (l.length <= n) Nil
      else {
        loop(this.tail(l), n - 1)
      }
    }

    l match {
      case Nil => Nil
      case _ => loop(l, n)
    }


  }

  def exercise3_5(): Unit = {
    def isEven(x: Int): Boolean = {
      x % 2 == 0
    }

    println(dropWhile(List(1, 2, 3, 4, 5, 6), isEven))
    println(dropWhile(List(2), isEven))
    println(dropWhile[Int](Nil, isEven))
  }


  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {

    def loop(keep: List[A], l: List[A], f: A => Boolean): List[A] = l match {
      case Nil => keep
      case ::(a: A, b: List[A]) =>
        if (f(a)) loop(::(a, keep), b, f)
        else loop(keep, b, f)
    }

    loop(Nil, l, f)
  }


  def exercise3_6(): Unit = {
    println(init(List(1, 2, 3, 4, 5, 6)))
      println(init(List(1)))
      println(init[Int](Nil))
  }

  def init[A](l: List[A]): List[A] = {

    def loop(keep: List[A], l: List[A]): List[A] = l match {
      case Nil => keep
      case ::(_,Nil) => keep
      case ::(a: A, b: List[A]) => loop(::(a, keep), b)
    }

    loop(Nil, l)
  }


}
