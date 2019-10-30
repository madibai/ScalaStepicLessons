import io.StdIn.readInt
import scala.math.BigDecimal.RoundingMode.HALF_UP

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


  //1
  val aaa: Double = normalDistribution(1.0,1.0,1.0)
  println(aaa)
  //2
  val aa21: BigDecimal = crispsWeight(90.0, 0.9, 0.1)
  val aa22: BigDecimal = crispsWeightMadi(90.0, 0.9, 0.1)
  println(aa21,aa22)
  //3
  //val x = readInt()
  //println(CountSymbolsInString(x))
  //4
  //val s3 = "bar"; val s1 = "foo" + s3; val s2 = "foo" + s3; println(s1 == s2)
  //var ans: Boolean = isCapital("Scala",3)
  //println(ans)
  //5




  def normalDistribution(mu: Double, sigma: Double, xyz: Double): Double = {
    val res = 1 / (sigma * Math.sqrt(2 * Math.PI)) * Math.pow(Math.E, -(Math.pow((xyz - mu), 2) / (2 * sigma * sigma)))
    res
  }
  def crispsWeight(weight: BigDecimal, potatoWaterRatio: Double, crispsWaterRatio: Double): BigDecimal = {
    val vl: BigDecimal = weight*(1-potatoWaterRatio)/(1-crispsWaterRatio)
    vl.setScale(5, math.BigDecimal.RoundingMode.HALF_UP)
    vl
  }
  def crispsWeightMadi(weight: BigDecimal, potatoWaterRatio: Double, crispsWaterRatio: Double): BigDecimal = {
    val vl: BigDecimal = (weight * (1 - potatoWaterRatio) * crispsWaterRatio) / (1 - crispsWaterRatio) + weight * (1 - potatoWaterRatio)
    vl.setScale(5, math.BigDecimal.RoundingMode.HALF_UP)
    vl
  }
  def CountSymbolsInString(inputString: Int):Int={
    val count = inputString.toBinaryString.count(_ == '1') //result 2 esli vvesti 2
    count
  }
  def isCapital(word: String, pos: Int): Boolean = {
    var answer = false
    if (word.charAt(pos).isUpper){
      answer = true
    }
    answer
  }
}
