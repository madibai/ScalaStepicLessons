import io.StdIn.readInt
import io.StdIn.readLine
import scala.math.BigDecimal.RoundingMode.HALF_UP
import scala.util.matching.Regex
import scala.math.abs
import scala.annotation.tailrec

object Example extends App {
  //val messg = "dasdasd"
  //print(messg)
  /*
  val x = readInt()
  val y = readInt()
  val z = readInt()
  mmm()
  def mmm(){
    println(x+y)
  }
  println(x*y*z)
  val msg: String = "hello Lol"
  val msg2: Double = {
    val x = 1.0
    Math.cos(x)
  }
  val msg3: Unit = Math.acos(1.0)
  println(msg2)
  println(msg3)
  val msg4: AnyVal = 42
  val gh: Float = 1.4F
  val weight: BigDecimal = 70.0
  val potatoWaterRatio: Double = 0.9
  val crispsWaterRatio: Double = 0.1
  val vse: BigDecimal = (weight*(1-potatoWaterRatio)*crispsWaterRatio)/(1-crispsWaterRatio)+weight*(1-potatoWaterRatio)
  println(vse.setScale(5,math.BigDecimal.RoundingMode.HALF_UP))
  val vse2: BigDecimal = weight*(1-potatoWaterRatio)/(1-crispsWaterRatio)
  println(vse2.setScale(5,math.BigDecimal.RoundingMode.HALF_UP))
  */
  /*
  //1
  val aaa: Double = normalDistribution(1.0,1.0,1.0)
  println(aaa)
  //2
  val aa21: BigDecimal = crispsWeight(90.0, 0.9, 0.1)
  val aa22: BigDecimal = crispsWeightMadi(90.0, 0.9, 0.1)
  println(aa21,aa22)
  */
  //3
  //val x = readInt()
  //println(CountSymbolsInString(x))
  //4
  //val s3 = "bar"; val s1 = "foo" + s3; val s2 = "foo" + s3; println(s1 == s2)
  //var ans: Boolean = isCapital("Scala",3)
  //println(ans)
  //5
  /*
  val twodigits = readLine()
  val start = twodigits.split(" ")(0).toInt
  val end = twodigits.split(" ")(1).toInt
  val toInvert = readLine()
  val invertStr: String = invertSomeString(start,end,toInvert.strip())
  println(invertStr)
  */
  //6
  /*
  val x = readLine()
  val ans: Boolean = checkSnakeCase(x)
  println(ans)
  */
  //7
  //val ans: Int = fibIter(0,0,2)
  //println(ans)
  //val answer: Int = fibIterLoop(2)
  //println(answer)
  //println(fib(7))
  //8
  /*
  val mul3 = 3*(_: Double)
  val pow2 = (x: Double) => x*x
  println((pow2.andThen[Double] _ )(mul3)(5))//=75
  println((mul3 andThen pow2)(5))//=225
  println(pow2.compose(mul3)(5))//=225
  println(mul3.compose(pow2)(5))//=75
  */
  //9
  //val searchOdd = LessonData.searchInArray(_ % 2 == 1, _: List[Int])
  //println(searchOdd(List(8,11,12))) // List(11)
  //10
  //println(fibs(100))
  //11
  //println(middle("scala"))
  //println(middle("123456"))
  //12
  //for {i<-1 to 10; j<-1 to 10}println(i+j)
  //for (i <- Range.inclusive(1,10)) println(i)
  //13
  /*
  val x = readInt()
  for {
    i <- Range(1, x)
    j <- Range(1, x) if BigInt(i).gcd(BigInt(j))==1
  } println(s"$i $j")
  */
  //14
  /*
  println("Alexander" match {
    case "Alexey" => 1
    case "alexander" => 2
    case "Alex" => 3
    case "Oleg" => 4
    case _ => 100
  })
  */
  //15
  /*
  case class Pet(name: String, says: String)
  val pet = Pet("dsa","010111")
  val check = "[0-1]{1,}".r
  val kind = pet match {
    case Pet(_, "meow" | "nya") => "cat"
    case Pet("Rex", _) => "dog"
    case Pet(_, check()) => "robot"
    case Pet(_,_) => "unknown"
  }
  println(kind)
  */
  //16
  val input: List[String] = List("oleg oleg@email.com",".", "oleg@email.com","asdgmaiasdl.com","a6sd6da7sda9")
  val name_reg = "[a-zA-Z]+".r
  val mail_reg = "\\w+@\\w+\\.\\w+".r
  val domain_reg = "@([-0-9a-zA-Z.+_]+)".r
  val Email = "([-0-9a-zA-Z.+_]+)@([-0-9a-zA-Z.+_]+)".r

  //val asd = input.foreach{println()}

  val result: List[String] = input.map {x =>
    x match {
      case domain_reg(dom) => dom
      case Email(name,domain) => name + " " + domain
      case mail_reg(test) => test
      case name_reg() => x
      //case mail_reg() => (x.substring(x.indexOf('@') + 1, x.length))
      case _ => ""
    }
  }.filter(_.nonEmpty)
  println(result mkString " ")
/*
  val result2 = input.map {x=> x.split(" ") =>
    z match {
      case name_reg() => ""
  }}

 */

  def normalDistribution(mu: Double, sigma: Double, xyz: Double): Double = {
    val res = 1 / (sigma * Math.sqrt(2 * Math.PI)) * Math.pow(Math.E, -(Math.pow((xyz - mu), 2) / (2 * sigma * sigma)))
    res
  }

  def crispsWeight(weight: BigDecimal, potatoWaterRatio: Double, crispsWaterRatio: Double): BigDecimal = {
    val vl: BigDecimal = weight * (1 - potatoWaterRatio) / (1 - crispsWaterRatio)
    vl.setScale(5, math.BigDecimal.RoundingMode.HALF_UP)
    vl
  }

  def crispsWeightMadi(weight: BigDecimal, potatoWaterRatio: Double, crispsWaterRatio: Double): BigDecimal = {
    val vl: BigDecimal = (weight * (1 - potatoWaterRatio) * crispsWaterRatio) / (1 - crispsWaterRatio) + weight * (1 - potatoWaterRatio)
    vl.setScale(5, math.BigDecimal.RoundingMode.HALF_UP)
    vl
  }

  def CountSymbolsInString(inputString: Int): Int = {
    val count = inputString.toBinaryString.count(_ == '1') //result 2 esli vvesti 2
    count
  }

  def isCapital(word: String, pos: Int): Boolean = {
    var answer = false
    if (word.charAt(pos).isUpper) {
      answer = true
    }
    answer
  }

  def invertSomeString(start: Int, end: Int, x: String): String = {
    //val ans = x.charAt(start)
    var dd = ""
    if (end >= x.length) {
      dd = x.substring(0, start) + x.substring(start, x.length).reverse + x.substring(x.length, x.length)
    } else {
      dd = x.substring(0, start) + x.substring(start, end + 1).reverse + x.substring(end + 1, x.length)
    }
    dd
  }

  def checkSnakeCase(inputString: String): Boolean = {
    var ans = false
    val madi = "^[a-z]{1,}(_[a-z]{1,}){0,}$"
    ans = (inputString.matches(madi))
    ans
  }

  def fibIter(prev: Int, current: Int, n: Int): Int = {
    if (n == 0) current
    else fibIter(current, prev + current, n - 1)
  }

  def fibIterLoop(num: Int): Int = {
    var ans = 0
    if (num == 2 & num == 1) {
      ans = 1
    } else {
      ans = 1
      var i = 2
      var prev = 1
      while (i < num) {
        val sum = prev + ans
        prev = ans
        ans = sum
        i += 1
      }
    }
    ans
  }

  def fib(n: Int): Int = {
    if (n > 1) fib(n - 1) + fib(n - 2)
    else n
  }

  def fibs(n: Int, currentNumber: Int = 1, f1: BigInt = 0, f2: BigInt = 1): BigInt = {
    if (n == currentNumber)
      f2
    else {
      fibs(n, currentNumber + 1, f2, f1 + f2)
    }
  }

  def middle[A](data: => Iterable[A]): A = {
    if (data.size % 2 == 0 && data.nonEmpty) {
      val tuple = data.splitAt(data.size / 2)
      println(tuple)
      tuple._2.head
    }
    else if (data.size % 2 != 0 && data.nonEmpty) {
      val tuple = data.splitAt((data.size - 1) / 2)
      println(tuple)
      tuple._2.head
    }
    else {
      None.asInstanceOf[A]
    }
  }

}
object LessonData{
  def searchInArray(cond: Int => Boolean, array: List[Int]): List[Int] = {
    array.filter(cond)
  }
}
