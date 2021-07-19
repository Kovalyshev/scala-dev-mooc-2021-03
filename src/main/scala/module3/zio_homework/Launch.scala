package module3.zio_homework

import zio.console.Console
import zio.random.Random

object Launch {
  def main(args: Array[String]): Unit = {
    zio.Runtime.default.unsafeRun(appSpeedUp2)
  }
}
