package functional.programming.chapters.three

object Exercises {


  //  def main(args: Array[String]): Unit = {
  //    exercise3_1()
  //    exercise3_8()
  //  }


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


  //3.2
  def tail[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case ::(_, a) => a
  }


  //3.3
  def setHead[A](list: List[A], item: A): List[A] = list match {
    case Nil => Nil
    case ::(_, a) => item :: a
  }


  //3.4
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


  //3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {

    l match {
      case Nil => Nil
      case ::(a: A, b: List[A]) =>
        if (f(a)) ::(a, dropWhile(b, f))
        else dropWhile(b, f)
    }
  }


  //3.6
  //    init(Cons(1, Cons(2, Cons(3, Nil)))
  //    ::(1,init(Cons(2, Cons(3, Nil)))
  //    ::(1,::(2, init(3, Nil)))
  //    ::(1,::(2, Nil))
  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case ::(_, Nil) => Nil
      case ::(a: A, b: List[A]) => ::(a, init(b))
    }
  }


  def exercise3_8(): Unit = {
    println(init(foldRight(List(1, 2, 3), Nil: List[Int])(::(_, _))))
  }


  //3.9
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case ::(x, xs) => f(x, foldRight(xs, z)(f))
  }

  //3.10
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case ::(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }


  def addOne(list: List[Int]): List[Int] = {
    list match {
      case Nil => Nil
      case ::(x, y) => ::(x+1, addOne(y))
    }
  }

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def size[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(x) => 1
      case Branch(l,r) => 1 + size(l) + size(r)
    }
  }

  def maximum(tree: Tree[Int]): Int = {
    tree match {
      case Leaf(x) => x
      case Branch(l,r) => this.max(maximum(l), maximum(r))
    }
  }

  def max(x: Int, y: Int):Int= {
    if(x>=y) x
    else y
  }

  def depth(tree: Tree[Int]): Int = {
    tree match {
      case Leaf(x) => 1
      case Branch(l,r) => 1 + this.max(depth(l),depth(r))
    }
  }


}
