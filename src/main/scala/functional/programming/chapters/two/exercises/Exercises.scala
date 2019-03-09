package functional.programming.chapters.two.exercises

import scala.annotation.tailrec

object Exercises {

  def main(args: Array[String]): Unit = {
    //exercise2_1()
    //exercise2_2()


  }

  //Exercise 2.1
  def exercise2_1(): Unit ={
    println(fib(0))
    println(fib(1))
    println(fib(2))
    println(fib(3))
    println(fib(4))
    println(fib(5))
  }

  //Exercise 2.2
    def exercise2_2(): Unit = {
      val person = new Person(10,"joe");
      val person1 = new Person(23,"jay");
      val person2 = new Person(52,"rob");
      val non_sortedArray = Array(person1,person2,person2);
      println(isSorted(non_sortedArray,ordered))

      val sortedArray = Array(person,person1,person2);
      println(isSorted(sortedArray,ordered))
    }

  //2.3
  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    (a:A) => ((b:B)=>f(a,b))


  //2.4
  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a,b) => (f(a)(b))

  //2.5
  def compose[A,B,C](f: B => C, g: A => B): A => C =
    a => f(g(a))



    def fib(n: Int): Int ={
    @annotation.tailrec
    def go(last: Int, current: Int, iter: Int): Int ={
      if (iter==n) current
      else go(current, last+current,iter+1)
    }
    if (n==0) 0
    else go(0,1,1)
  }


  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean= {
    @tailrec
      def go(iter:Int):Boolean = {
        if(iter==as.length-1) true
        else if (ordered(as(iter),as(iter+1))) go(iter+1)
        else false
      }
    go(0)
  }

  def ordered(first:Person, second:Person): Boolean ={
    first.getAge()<second.getAge()
  }






}
