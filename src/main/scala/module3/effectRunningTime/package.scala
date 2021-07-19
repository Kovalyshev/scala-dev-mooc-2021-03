package module3

import module3.zioConcurrency.currentTime
import zio.clock.Clock
import zio.console.{Console, putStrLn}
import zio.{Has, ULayer, URIO, ZIO, ZLayer, clock}
import zio.macros.accessible

import java.util.concurrent.TimeUnit

package object effectRunningTime {

  @accessible
  object EffectRunningTimeService {
    type EffectRunningTimeService = Has[EffectRunningTimeService.Service]

    trait Service {
      def currentTime: URIO[Clock, Long]
      def printEffectRunningTime[R, E, A](eff: => ZIO[R, E, A]): ZIO[Console with Clock with R, E, A]
    }

    val live: ULayer[EffectRunningTimeService] = ZLayer.succeed(new Service {
      override def currentTime: URIO[Clock, Long] =
        clock.currentTime(TimeUnit.SECONDS)

      override def printEffectRunningTime[R, E, A](eff: => ZIO[R, E, A]): ZIO[Console with Clock with R, E, A] =
        for {
          start <- currentTime
          r <- eff
          finish <- currentTime
          _ <- putStrLn(s"Running time ${finish - start}")
        } yield r
    })
  }
}
