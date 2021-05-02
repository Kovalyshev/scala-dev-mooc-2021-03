package module1

object functions {


  /**
   * Функции
   */



  // SAM Single Abstract Method


  // trait Function1[Int, Int]{ def apply(x: Int): Int}



  /**
   *  Задание 1. Написать ф-цию метод isEven, которая будет вычислять является ли число четным
   */
  def isEven(number: Int): Boolean = number % 2 == 0


  /**
   * Задание 2. Написать ф-цию метод isOdd, которая будет вычислять является ли число нечетным
   */
  def isOdd(number: Int): Boolean = !isEven(number)


  /**
   * Задание 3. Написать ф-цию метод filterEven, которая получает на вход массив чисел и возвращает массив тех из них,
   * которые являются четными
   */
  def filterEven(numbers: List[Int]): List[Int] = numbers.filter(num => isEven(num))

  def filterEven2(numbers: List[Int]): List[Int] = filterRec(numbers)(isEven)

  /**
   * Задание 3. Написать ф-цию метод filterOdd, которая получает на вход массив чисел и возвращает массив тех из них,
   * которые являются нечетными
   */
  def filterOdd(numbers: List[Int]): List[Int] = numbers.filter(num => isOdd(num))

  def filterOdd2(numbers: List[Int]): List[Int] = filterRec(numbers)(isOdd)


  /**
   * Метод работает через прием условия и рекурсию
   *
   * @param numbers числа
   * @param condition условие
   * @return результирующий список
   */
  def filterRec(numbers: List[Int])(condition: Int => Boolean): List[Int] = {
    def checkCondition(nums: List[Int], cond: Int => Boolean, result: List[Int]): List[Int] = {
      val h = nums.head
      val tails = nums.tail
      if (tails.isEmpty) {
        if (cond(nums.head)) {
          nums.head :: result
        } else {
          result
        }
      } else {
        val res2 = {
          if (cond(nums.head)) {
            nums.head :: result
          } else {
            result
          }
        }
        checkCondition(nums.tail, cond, res2)
      }
    }

    checkCondition(numbers, condition, List())
  }

  /**
   * return statement
   *
   *
   * val two = (x: Int) => { return x; 2 }
   *
   *
   * def sumItUp: Int = {
   *    def one(x: Int): Int = { return x; 1 }
   *    val two = (x: Int) => { return x; 2 }
   *    1 + one(2) + two(3)
   * }
   */



}
