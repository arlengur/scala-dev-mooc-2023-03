package module3

import module3.zioConcurrency.{currentTime, printEffectRunningTime}
import module3.zio_homework.MyPrintService.MyPrint
import zio.{Has, IO, Task, ULayer, ZIO, ZLayer, clock}
import zio.clock.{Clock, sleep}
import zio.console._
import zio.duration.durationInt
import zio.macros.accessible
import zio.random._

import java.io.IOException
import java.util.concurrent.TimeUnit
import scala.io.StdIn
import scala.language.postfixOps

package object zio_homework {
  /**
   * 1.
   * Используя сервисы Random и Console, напишите консольную ZIO программу которая будет предлагать пользователю угадать число от 1 до 3
   * и печатать в когнсоль угадал или нет. Подумайте, на какие наиболее простые эффекты ее можно декомпозировать.
   */


  lazy val guessProgram = for {
    _ <- ZIO.effect(println("Угадайте число от 1 до 3"))
    in <- ZIO.effect(StdIn.readLine().toInt)
    rnd <- zio.random.nextIntBetween(1, 3)
    _ <- ZIO.effect(if (in == rnd) println("Верно") else println("Неправильно"))
  } yield ()

  /**
   * 2. реализовать функцию doWhile (общего назначения), которая будет выполнять эффект до тех пор, пока его значение в условии не даст true
   *
   */

  def doWhile(cond: Int => Boolean, a: Int): Task[Unit] = if (cond(a)) ZIO.succeed() else
    ZIO.effect(println(a)) *> doWhile(cond, a + 1)


  /**
   * 3. Реализовать метод, который безопасно прочитает конфиг из файла, а в случае ошибки вернет дефолтный конфиг
   * и выведет его в консоль
   * Используйте эффект "load" из пакета config
   */


  def loadConfigOrDefault = config.load.
    flatMap(conf => ZIO.effect(println(conf)))
    .orElse(ZIO.effect(println("Default config")))


  /**
   * 4. Следуйте инструкциям ниже для написания 2-х ZIO программ,
   * обратите внимание на сигнатуры эффектов, которые будут у вас получаться,
   * на изменение этих сигнатур
   */


  /**
   * 4.1 Создайте эффект, который будет возвращать случайеым образом выбранное число от 0 до 10 спустя 1 секунду
   * Используйте сервис zio Random
   */
  lazy val eff = ZIO.sleep(1 seconds) *> zio.random.nextIntBetween(0, 10)

  /**
   * 4.2 Создайте коллекцию из 10 выше описанных эффектов (eff)
   */
  lazy val effects = List.fill(10)(eff)


  /**
   * 4.3 Напишите программу которая вычислит сумму элементов коллекци "effects",
   * напечатает ее в консоль и вернет результат, а также залогирует затраченное время на выполнение,
   * можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях
   */

  lazy val app = printEffectRunningTime(for {
    v <- ZIO.collectAll(effects)
    _ <- ZIO.effect(println(v.sum))
    res <- ZIO.succeed(v.sum)
  } yield res)


  /**
   * 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
   */

  lazy val appSpeedUp = printEffectRunningTime(for {
    v <- ZIO.collectAllPar(effects)
    _ <- ZIO.effect(println(v.sum))
    res <- ZIO.succeed(v.sum)
  } yield res)


  /**
   * 5. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
   * молжно было использовать аналогично zio.console.putStrLn например
   */

  object MyPrintService {
    trait Service {
      def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[R with Clock, E, A]
    }

    class ServiceImpl extends Service {
      def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[R with Clock, E, A] =
        for {
          start <- clock.currentTime(TimeUnit.SECONDS)
          r <- zio
          end <- clock.currentTime(TimeUnit.SECONDS)
          _ <- ZIO.effect(println(s"Running time ${end - start}")).orDie
        } yield r
    }

    type MyPrint = Has[Service]

    val live: ULayer[MyPrint] = ZLayer.succeed(new ServiceImpl)

    def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[R with Clock with MyPrint, E, A] =
      ZIO.environment[MyPrint].flatMap(_.get printEffectRunningTime zio)
  }

  /**
   * 6.
   * Воспользуйтесь написанным сервисом, чтобы созадть эффект, который будет логировать время выполнения прогаммы из пункта 4.3
   */

  lazy val appWithTimeLogg = MyPrintService.printEffectRunningTime(
    for {
      v <- ZIO.collectAllPar(effects)
      _ <- ZIO.effect(println(s"sum = ${v.sum}"))
      res <- ZIO.succeed(v.sum)
    } yield res
  )

  /**
   * Подготовьте его к запуску и затем запустите воспользовавшись ZioHomeWorkApp
   */

  val env = MyPrintService.live
  lazy val runApp = appWithTimeLogg.provideSomeLayer[Random with Clock](env)

}
