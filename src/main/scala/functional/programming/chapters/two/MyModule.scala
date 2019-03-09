package functional.programming.chapters.two

object MyModule {

  def abs(n : Int): Int =
    if(n<0) -n
    else n


  def main(args: Array[String]): Unit = {
    println(formatResult("factorial", 5,factorial))
    println(formatResult("absolute value", -5,abs))

  }


  def factorial(n: Int): Int ={
    def facts(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else facts(n - 1,n*acc)
    }
    facts(n,1)

  }


  def formatResult(name: String, n: Int, f: Int => Int)  = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

}
