package coop.rchain.node
import cats.Eq
import cats.data.EitherT
import cats.tests.CatsSuite
import cats.effect.laws.discipline.SyncTests
import cats.laws.discipline.arbitrary._
import coop.rchain.comm.CommError
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global

import scala.concurrent.duration._
import cats.implicits._
import org.scalacheck.{Arbitrary, Gen}

class EffectSpec extends CatsSuite {
  implicit val arbCommError: Arbitrary[CommError] = Arbitrary(
    Gen.alphaStr.map(s => CommError.unknownCommError(s))
  )
  implicit def arbEffect[T: Arbitrary]: Arbitrary[Effect[T]] =
    Arbitrary(
      Arbitrary
        .arbitrary[T]
        .flatMap(
          t => Gen.oneOf(Right(t), Left(CommError.unknownCommError("some error")))
        )
        .map(either => EitherT[Task, CommError, T](Task.delay(either)))
    )

  implicit val eqThrowable: Eq[Throwable] =
    Eq.instance((t1: Throwable, t2: Throwable) => Eq[String].eq(t1.getMessage, t2.getMessage))

  implicit val eqCommError: Eq[CommError] = Eq.instance((e1, e2) => e1.message == e2.message)

  implicit def eqEither[E: Eq, T: Eq]: Eq[Either[E, T]] =
    Eq.instance(
      (e1, e2) =>
        (e1.isLeft && e2.isLeft && Eq[E].eq(e1.left.get, e2.left.get))
          || (e1.isRight && e2.isRight && Eq[E].eq(e1.right.get, e2.right.get))
    )

  implicit def eqEffect[T: Eq]: Eq[Effect[T]] =
    Eq.instance(
      (eff1: Effect[T], eff2: Effect[T]) =>
        (for {
          v1 <- eff1.value
          v2 <- eff2.value
        } yield Eq[Either[CommError, T]].eq(v1, v2)).runSyncUnsafe(10.seconds)
    )

  checkAll("Sync[Effect] laws", SyncTests[Effect](syncEffect).sync[Int, Int, Int])
}
