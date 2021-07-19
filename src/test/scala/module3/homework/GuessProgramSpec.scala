package module3.homework

import zio.test._
import zio.test.Assertion.{containsString, equalTo, hasSize}
import zio.test.{DefaultRunnableSpec, ZSpec, suite}
import module3.zio_homework.guessProgram
import zio.test.environment.TestConsole

object GuessProgramSpec extends DefaultRunnableSpec {
  override def spec: ZSpec[_root_.zio.test.environment.TestEnvironment, Any] = {
    suite("Тестирование Угадай число")(
      testM("Основная логика")(
        for {
          _ <- TestConsole.feedLines("2")
          _ <- guessProgram
          lines <- TestConsole.output
        } yield assert(lines)(hasSize(equalTo(2))) &&
          (assert(lines(1))(containsString("Вы угадали число")) ||
            assert(lines(1))(containsString("Вы не угадали число")))
      )
    )
  }
}
