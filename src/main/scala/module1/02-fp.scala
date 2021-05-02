package module1


import java.util.UUID
import scala.annotation.tailrec


/**
 * referential transparency
 */
 object referential_transparency{


  case class Abiturient(id: String, email: String, fio: String)

  type Html = String

  sealed trait Notification
  object Notification{
    case class Email(email: String, text: Html) extends Notification
    case class Sms(telephone: String, msg: String) extends Notification
  }


  case class AbiturientDTO(email: String, fio: String, password: String)

  trait NotificationService{
    def sendNotification(notification: Notification): Unit
  }

  trait AbiturientService{

    def registerAbiturient(uuid: String, abiturientDTO: AbiturientDTO): Abiturient
  }

  class AbiturientServiceImpl(notificationService: NotificationService) extends AbiturientService{

    override def registerAbiturient(uuid: String, abiturientDTO: AbiturientDTO): Abiturient = {
      val abiturient = Abiturient(uuid, abiturientDTO.email, abiturientDTO.fio)
      notificationService.sendNotification(Notification.Email(abiturient.email, "Some message"))
      abiturient
    }
    def registerAbiturient2(abiturientDTO: AbiturientDTO): (Abiturient, Notification) = {
      val abiturient = Abiturient(UUID.randomUUID().toString, abiturientDTO.email, abiturientDTO.fio)
      (abiturient, Notification.Email(abiturient.email, "Some message"))
    }

  }
}


 // recursion

object recursion {

  /**
   * Реализовать метод вычисления n!
   * n! = 1 * 2 * ... n
   */

  def fact(n: Int): Int = {
    var _n = 1
    var i = 2
    while (i <= n) {
      _n = _n *  i
      i = i + 1
    }
    _n
  }

  def fact2(n: Int): Int = {
    if(n <= 1) 1
    else n * fact2(n - 1)
  }

  def fact3(n: Int): Int = {

    @tailrec
    def loop(n1: Int, acc: Int): Int ={
      if(n1 <= 1) acc
      else loop(n1 - 1, n1 * acc)
    }
    loop(n, 1)
  }


  /**
   * реализовать вычисление N числа Фибоначчи
   * F0 = 0, F1 = 1, Fn = Fn-1 + Fn - 2
   *
   */

   def fib(n: Int): Int = fib(n -1) + fib(n - 2)

}

object hof{

  def printFactorialResult(r: Int) = println(s"Factorial result is ${r}")

  def printFibonacciResult(r: Int) = println(s"Fibonacci result is ${r}")

  def printResult(r: Int, name: String) = println(s"$name result is ${r}")


  def printFuncResult[A, B](f: A => B, v: A, name: String) =
    println(s"$name result is ${f(v)}")





  // Follow type implementation
  def partial[A,B,C](a: A, f: (A, B) => C): B => C = (b : B) => f(a, b) // B => C

  def sum(x: Int, y: Int): Int = x + y

  val r: Int => Int = partial(1, sum)

  r(2) // sum(1, 2)


}






/**
 *  Реализуем тип Option
 */


 object opt {

  /**
   *
   * Реализовать тип Option, который будет указывать на присутствие либо отсутсвие результата
   */

   // Animal
   // Dog extend Animal
  // Option[Dog] Option[Animal]

   sealed trait Option[+A]{
    def isEmpty: Boolean = this match {
      case Option.Some(_) => false
      case Option.None => true
    }

    def get: A = this match {
      case Option.Some(v) => v
      case Option.None => throw new Exception("Get on empty list")
    }


    def getOrElse[B >: A](b: B): B = this match {
      case Option.Some(v) => v
      case Option.None => b
    }

    def map[B](f: A => B): Option[B] = this match {
      case Option.Some(v) => Option.Some(f(v))
      case Option.None => Option.None
    }

    def flatMap[B](f: A => Option[B]): Option[B] = this match {
      case Option.None => Option.None
      case Option.Some(v) =>
        f(v)
    }

    // val i : Option[Int]  i.map(v => v + 1)


    def f(x: Int, y: Int): Option[Int] =
      if(y == 0) Option.None
      else Option.Some(x / y)


    def printIfAny(): Unit = {
      this match {
        case Option.Some(v) =>
          println(v)
        case Option.None =>
      }
    }

    def orElse[B >: A](other: Option[B]): Option[B] = {
      this match {
        case Option.None =>
          other
        case Option.Some(v) =>
          Option.Some(v)
      }
    }

    /*def get: A = {
      this match {
        case Option.None =>
          throw new Exception("Not data in")
        case Option.Some(v) =>
          v
      }
    }*/

    def zip[B](other: Option[B]): Option[(A, B)] = {
      (this, other) match {
        case (Option.Some(a), Option.Some(b)) =>
          Option.Some((a, b))
        case (Option.None, Option.Some(_)) | (Option.Some(_), Option.None) =>
          Option.None
      }
    }

    def filter(cond: A => Boolean): Option[A] = {
      this match {
        case some @ Option.Some(v) if cond(v) =>
          some
        case _ =>
          Option.None
      }
    }
  }

   object Option{
     case class Some[A](v: A) extends Option[A]
     case object None extends Option[Nothing]
   }

  def main(args: Array[String]): Unit = {

  }

  /**
   *
   * Реализовать метод printIfAny, который будет печатать значение, если оно есть
   */
  println("Тест реализации метода printIfAny")
  val op1Has = Option.Some(5)
  val op1None = Option.None
  op1Has.printIfAny()
  op1None.printIfAny()

  /**
   *
   * реализовать метод orElse который будет возвращать другой Option, если данный пустой
   */
    println("Тест реализации метода orElse")
  val op2Has = Option.Some("Fix")
  val op2None = Option.None
  val op2Def = Option.Some("Update")
  println(op2Has.orElse(op2Def))
  println(op2None.orElse(op2Def))

  /**
   *
   * Реализовать метод isEmpty, который будет возвращать true если Option не пуст и false в противном случае
   */
  println("Тест реализации метода isEmpty")
  val op3Has = Option.Some(35)
  val op3None = Option.None
  println(s"Not empty is ${op3Has.isEmpty}")
  println(s"Empty is ${op3None.isEmpty}")

  /**
   *
   * Реализовать метод get, который будет возвращать значение
   */
  println("Тест реализации метода get")
  val op4Has = Option.Some(35)
  val op4None = Option.None
  try {
    println(s"Not empty value is ${op4Has.get}")
    println(s"Empty value is ${op4None.get}")
  }
  catch {
    case _: Exception =>
      println("Getting value of None with exception is normal")
  }

  /**
   *
   * Реализовать метод zip, который будет создавать Option от пары значений из 2-х Option
   */
  println("Тест реализации метода zip")
  val op5Has1 = Option.Some(35)
  val op5Has2 = Option.Some("Fix")
  val op5None = Option.None
  try {
    println(s"Not empty with not empty is ${op5Has1.zip(op5Has2)}")
    println(s"Not empty with empty is ${op5Has1.zip(op5None)}")
  }
  catch {
    case _: Exception =>
      println("Getting value of None with exception is normal")
  }

  /**
   *
   * Реализовать метод filter, который будет возвращать не пустой Option
   * в случае если исходный не пуст и предикат от значения = true
   */
  println("Тест реализации метода filter")
  val op6Has = Option.Some(35)
  val op6None = Option.None
  println(s"Not empty value filter with true condition is ${op6Has.filter(_ > 0)}")
  println(s"Not empty value filter with false condition is ${op6Has.filter(_ < 0)}")
  println(s"Empty value filter with any condition is ${op6Has.filter(_ > 0)}")

}