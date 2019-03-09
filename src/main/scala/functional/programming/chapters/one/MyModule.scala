package functional.programming.chapters.one

object MyModule {

  def abs(n : Int): Int =
    if(n<0) -n
    else n


  def main(args: Array[String]): Unit = {
    println(abs(-1));
    println(abs(2));
    println(factorial(4))
  }


  def factorial(n: Int): Int ={
    def facts(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else facts(n - 1,n*acc)
    }
    facts(n,1)

  }


}
