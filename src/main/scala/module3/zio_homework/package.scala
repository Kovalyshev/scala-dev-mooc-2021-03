package module3

import module3.effectRunningTime.EffectRunningTimeService
import module3.effectRunningTime.EffectRunningTimeService.EffectRunningTimeService
import module3.zioConcurrency.printEffectRunningTime
import zio.clock.Clock
import zio.{Ref, Schedule, ULayer, ZIO}
import zio.random.{Random, nextIntBetween}
import zio.console._
import zio.duration.durationInt

import java.io.IOException
import scala.language.postfixOps

package object zio_homework {
  /**
   * 1.
   * Используя сервисы Random и Console, напишите консольную ZIO программу которая будет предлагать пользователю угадать число от 1 до 3
   * и печатать в когнсоль угадал или нет.
   */
  lazy val guessProgram: ZIO[Random with Console, IOException, Unit] = for {
    console <- ZIO.environment[Console].map(_.get)
    random <- ZIO.environment[Random].map(_.get)
    guessNumber <- random.nextIntBetween(1, 4)
    _ <- console.putStrLn("Угадайте число введя число от 1 до 3")
    inNumber <- console.getStrLn.map(_.toInt)
    _ <- if (inNumber == guessNumber)
      putStrLn("Вы угадали число")
    else
      putStrLn("Вы не угадали число")
  } yield ()


  /**
   * 2. реализовать функцию doWhile, которая будет выполнять эффект до тех пор, пока его значение в условии не даст true
   */

  def doWhile[R, E, A](body: ZIO[R, E, A])(condition: A => Boolean): ZIO[R with Clock, E, A] = {
    val schedule = Schedule.recurWhile(condition)
    body.repeat(schedule)
  }

  /**
   * 3. Реализовать метод, который безопасно прочитает конфиг из файла, а в случае ошибки вернет дефолтный конфиг
   * и выведет его в консоль
   * Используйте эффект "load" из пакета config
   */

  def loadConfigOrDefault: ZIO[Any, Throwable, config.AppConfig] = {
    config.load.onError(_ => ZIO.succeed(config.AppConfig("app-name", "app-url")))
  }


  /**
   * 4. Следуйте инструкциям ниже для написания 2-х ZIO программ,
   * обратите внимание на сигнатуры эффектов, которые будут у вас получаться,
   * на изменение этих сигнатур
   */


  /**
   * 4.1 Создайте эффект, который будет возвращать случайеым образом выбранное число от 0 до 10 спустя 1 секунду
   * Используйте сервис zio Random
   */

  val eff: ZIO[Random with Clock, Nothing, Int] = for {
    _ <- ZIO.sleep(1 second)
    random <- ZIO.environment[Random].map(_.get)
    randNumber <- random.nextIntBetween(0, 11)
  } yield randNumber

  /**
   * 4.2 Создайте коллукцию из 10 выше описанных эффектов (eff)
   */
  lazy val effects: List[ZIO[Random with Clock, Nothing, Int]] =
    (1 to 10).toList.map(_ => eff)


  /**
   * 4.3 Напишите программу которая вычислит сумму элементов коллекци "effects",
   * напечатает ее в консоль и вернет результат, а также залогирует затраченное время на выполнение,
   * можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях
   */

  val sumEffect: ZIO[Console with Random with Clock, Nothing, Unit] = for {
    sum <- ZIO.foldLeft(effects)(0)(
      (sum, el) => el.map(_ + sum)
    )
    _ <- putStrLn(s"Сумма случайных чисел = $sum")
  } yield ()

  lazy val app: ZIO[Console with Clock with Random, Nothing, Unit] =
    printEffectRunningTime(sumEffect)

  /**
   * 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
   */

  val sumEffect2: ZIO[Console with Random with Clock, Nothing, Unit] = for {
    results <- ZIO.foreachPar(effects)(el => el)
    sum <- ZIO.succeed(results.sum)
    _ <- putStrLn(s"Сумма случайных чисел = $sum")
  } yield ()
  lazy val appSpeedUp: ZIO[Console with Clock with Random, Nothing, Unit] =
    printEffectRunningTime(sumEffect2)


  /**
   * 5. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
   * молжно было использовать аналогично zio.console.putStrLn например
   */
  val effPrinRunningTimeservice: ZIO[Console with Clock with Random with EffectRunningTimeService, Nothing, Unit] =
    for {
      runPrintTime <- ZIO.environment[EffectRunningTimeService].map(_.get)
      _ <- runPrintTime.printEffectRunningTime(sumEffect2)
    } yield ()

  val appSpeedUp2Env: ULayer[EffectRunningTimeService] =
    EffectRunningTimeService.live

  lazy val appSpeedUp2: ZIO[Console with Clock with Random, Nothing, Unit] =
    effPrinRunningTimeservice.provideSomeLayer[Console with Clock with Random](appSpeedUp2Env)
}
