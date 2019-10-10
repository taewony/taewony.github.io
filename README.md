## Functional Programming with Scala Dotty

### Dotty Version: 0.19.0-RC1

I'm doing self-learning about how to do FP in scala by trying to convert the existing scala codes into Dotty version of them. 

Here are three articles or projects re-written in Dotty, two are about `tagless final encoding` and the other is about `IO Monad`.


*  IO Monad by John A. De Goes,
[FP to the Max](https://gist.github.com/jdegoes/1b43f43e2d1e845201de853815ab3cb9).

*  Revisiting Tagless Final Interpreters by Olivier Blanvillain
[FTI](https://gist.github.com/OlivierBlanvillain/48bb5c66dbb0557da50465809564ee80).

*  Tagless Final in Scala: A Practical example by Juan Pablo Royo
[FP to the Max](https://gist.github.com/jdegoes/1b43f43e2d1e845201de853815ab3cb9).


IO Commands of Tagless Final in dotty
---
```code scala
package fpmax

import scala.util.Try
import scala.io.StdIn.readLine
import stdlib._
import stdlib.given

def msg = "I was compiled by dotty :)"

@main def IOEffect = {
  import App3._
  
  println("Hello world!")
  mainIO.unsafeRun()
}

object App3 {
  
  object ConsoleOut {
    case class YouGuessedRight(name: String) extends ConsoleOut {
      def en = "You guessed right, " + name + "!"
    }
    case class YouGuessedWrong(name: String, num: Int) extends ConsoleOut {
      def en = "You guessed wrong, " + name + "! The number was: " + num
    }
    case class DoYouWantToContinue(name: String) extends ConsoleOut {
      def en = "Do you want to continue, " + name + "?"
    }
    case class PleaseGuess(name: String) extends ConsoleOut {
      def en = "Dear " + name + ", please guess a number from 1 to 5:"
    }
    case class ThatIsNotValid(name: String) extends ConsoleOut {
      def en = "That is not a valid selection, " + name + "!"
    }
    case object WhatIsYourName extends ConsoleOut {
      def en = "What is your name?"
    }
    case class WelcomeToGame(name: String) extends ConsoleOut {
      def en = "Hello, " + name + ", welcome to the game!"
    }
  }

  case class TestData(input: List[String], output: List[ConsoleOut], nums: List[Int]) {
    def showResults = output.reverse.map(_.en).mkString("\n")
    def nextInt: (TestData, Int) = (copy(nums = nums.drop(1)), nums.head)
    def putStrLn(line: ConsoleOut): (TestData, Unit) = (copy(output = line :: output), ())
    def getStrLn: (TestData, String) = (copy(input = input.drop(1)), input.head)
  }

  case class TestIO[A](run: TestData => (TestData, A)) { self =>
    def map[B](f: A => B): TestIO[B] =
      TestIO(t => self.run(t) match { case (t, a) => (t, f(a)) })

    def flatMap[B](f: A => TestIO[B]): TestIO[B] =
      TestIO(t => self.run(t) match { case (t, a) => f(a).run(t) })

    def eval(t: TestData): TestData = self.run(t)._1
  }
  object TestIO {
    def pure[A](a: => A): TestIO[A] = TestIO(t => (t, a))

    given Random[TestIO] {
      def nextInt(upper: Int): TestIO[Int] =
        TestIO(t => t.nextInt)
    }
    given Program[TestIO] {
      def finish[A](a: A): TestIO[A] = TestIO.pure(a)
      def (fa: TestIO[A]) flatMap[A, B](afb: A => TestIO[B]): TestIO[B] = fa.flatMap(afb)
      def (fa: TestIO[A]) map[A, B](ab: A => B): TestIO[B] = fa.map(ab)
    }
    given Console[TestIO] {
      def putStrLn(line: ConsoleOut): TestIO[Unit] =
        TestIO(t => t.putStrLn(line))
      def getStrLn: TestIO[String] =
        TestIO(t => t.getStrLn)
    }
  }

  def parseInt(s: String): Option[Int] = Try(s.toInt).toOption

  def checkAnswer[F[_]: Console](name: String, num: Int, guess: Int): F[Unit] =
    if (num == guess) putStrLn(ConsoleOut.YouGuessedRight(name))
    else putStrLn(ConsoleOut.YouGuessedWrong(name, num))

  def checkContinue[F[_]: Program: Console](name: String): F[Boolean] =
    for {
      _       <- putStrLn(ConsoleOut.DoYouWantToContinue(name))
      choice  <- getStrLn.map(_.toLowerCase)
      cont    <- if (choice == "y") finish(true)
                 else if (choice == "n") finish(false)
                 else checkContinue(name)
    } yield cont

  def gameLoop[F[_]: Program: Console: Random](name: String): F[Unit] =
    for {
      num   <- nextInt(5).map(_ + 1)
      _     <- putStrLn(ConsoleOut.PleaseGuess(name))
      guess <- getStrLn
      _     <- parseInt(guess).fold(
                 putStrLn(ConsoleOut.ThatIsNotValid(name))
               )((guess: Int) => checkAnswer(name, num, guess))
      cont  <- checkContinue(name)
      _     <- if (cont) gameLoop(name) else finish(())
    } yield ()

  def main[F[_]: Program: Console: Random]: F[Unit] =
    for {
      _     <- putStrLn(ConsoleOut.WhatIsYourName)
      name  <- getStrLn
      _     <- putStrLn(ConsoleOut.WelcomeToGame(name))
      _     <- gameLoop(name)
    } yield ()

  def mainIO: IO[Unit] = main[IO]

  def mainTestIO: TestIO[Unit] = main[TestIO]

  val TestExample = TestData(
    input   = "john" :: "1" :: "n" :: Nil,
    output  = Nil,
    nums    = 0 :: Nil)
}


object stdlib {

  trait Program[F[_]] {
    def finish[A](a: A): F[A]
    def (fa: F[A]) flatMap[A, B](afb: A => F[B]): F[B]
    def (fa: F[A]) map[A, B](ab: A => B): F[B]
  }
  object Program {
    def apply[F[_]](given F: Program[F]): Program[F] = F
  }
  def finish[F[_], A](a: A)(given F: Program[F]): F[A] = F.finish(a)

  final case class IO[A](unsafeRun: () => A) { self =>
    final def map[B](f: A => B): IO[B] = IO(() => f(self.unsafeRun()))

    final def flatMap[B](f: A => IO[B]): IO[B] =
      IO(() => f(self.unsafeRun()).unsafeRun())
  }
  object IO {
    def pure[A](a: => A): IO[A] = IO(() => a)
  }
  given Program[IO] {
    def finish[A](a: A): IO[A] = IO.pure(a)
    def (fa: IO[A]) flatMap[A, B](afb: A => IO[B]): IO[B] = fa.flatMap(afb)
    def (fa: IO[A]) map[A, B](ab: A => B): IO[B] = fa.map(ab)
  }

  trait ConsoleOut {
    def en: String
  }

  trait Console[F[_]] {
    def putStrLn(line: ConsoleOut): F[Unit]
    def getStrLn: F[String]
  }
  object Console {
    def apply[F[_]](given F: Console[F]): Console[F] = F
  }
  given Console[IO] {
    def putStrLn(line: ConsoleOut): IO[Unit] = IO(() => println(line.en))
    def getStrLn: IO[String] = IO(() => readLine())
  }
  def putStrLn[F[_]: Console](line: ConsoleOut): F[Unit] = Console[F].putStrLn(line)
  def getStrLn[F[_]: Console]: F[String] = Console[F].getStrLn

  trait Random[F[_]] {
    def nextInt(upper: Int): F[Int]
  }
  object Random {
    def apply[F[_]](implicit F: Random[F]): Random[F] = F
  }
  given Random[IO] {
    def nextInt(upper: Int): IO[Int] = IO(() => scala.util.Random.nextInt(upper))
  }
  def nextInt[F[_]: Random](upper: Int): F[Int] = Random[F].nextInt(upper)
}
```

Expression of Tagless Final in dotty
---
```code scala
package fpmax

trait Exp[T] {
  def lit(i: Int): T
  def neg(t: T): T
  def add(l: T, r: T): T
}
object ExpSyntax {
  def lit[T](i: Int) (given e: Exp[T]): T = e.lit(i)
  def neg[T](t: T) (given e: Exp[T]): T = e.neg(t)
  def add[T](l: T, r: T) (given e: Exp[T]): T = e.add(l, r)
}
import ExpSyntax._

given Exp[Int] {
  def lit(i: Int): Int = i
  def neg(t: Int): Int = -t
  def add(l: Int, r: Int): Int = l + r
}
given Exp[String] {
  def lit(i: Int): String = i.toString
  def neg(t: String): String = s"(-$t)"
  def add(l: String, r: String): String = s"($l + $r)"
}

trait Mult[T] {
  def mul(l: T, r: T): T
}
object MultSyntax {
  def mul[T](l: T, r: T) (given e: Mult[T]): T = e.mul(l, r)
}
import MultSyntax._

given Mult[Int] {
  def mul(l: Int, r: Int): Int = l * r
}
given Mult[String] {
  def mul(l: String, r: String): String = s"$l * $r"
}

enum Tree {
  case Leaf(s: String) 
  case Node(s: String, ts: List[Tree])
}
enum NCtx {
  case PosCtx 
  case NegCtx
}
def pushNeg[T](e: NCtx => T): T = e(NCtx.PosCtx)

type ErrMsg = String
trait Wrapped {
  def value[T](given Exp[T]): T
}

given Exp[Tree] {
  def lit(i: Int): Tree = Tree.Node("Lit", List(Tree.Leaf(i.toString)))
  def neg(t: Tree): Tree = Tree.Node("NegCtx", List(t))
  def add(l: Tree, r: Tree): Tree = Tree.Node("Add", List(l , r))
}
given Mult[Tree] {
  def mul(l: Tree, r: Tree): Tree = Tree.Node("Mult", List(l , r))
}
def readInt(s: String): Either[ErrMsg, Int] = {
  import scala.util.{Try, Success, Failure}
  Try(s.toInt) match {
    case Success(i) => Right(i)
    case Failure(f) => Left(f.toString)
  }
}

def fromTree[T](t: Tree)(given e: Exp[T]): Either[ErrMsg, T] =
  t match {
    case Tree.Node("Lit", List(Tree.Leaf(n))) =>
      readInt(n).right.map(e.lit)

    case Tree.Node("Neg", List(t)) =>
      fromTree(t).right.map(e.neg)

    case Tree.Node("Add", List(l , r)) =>
      for(lt <- fromTree(l).right; rt <- fromTree(r).right)
      yield e.add(lt, rt)

    case _ => Left(s"Invalid tree $t")
  }

def fromTreeExt[T](recur: => (Tree => Either[ErrMsg, T]))(given Exp[T]): Tree => Either[ErrMsg, T] = {
  tree => tree match {
    case Tree.Node("Lit", List(Tree.Leaf(n))) =>
      readInt(n).right.map(lit)

    case Tree.Node("NegCtx", List(t)) =>
      recur(t).right.map(neg)

    case Tree.Node("Add", List(l , r)) =>
      for(lt <- recur(l).right; rt <- recur(r).right)
      yield add(lt, rt)

    case t => Left(s"Invalid tree $t")
  }
}

given Exp[Wrapped] {
  def lit(i: Int) = new Wrapped {
    def value[T](given e: Exp[T]): T = e.lit(i)
  }
  def neg(t: Wrapped) = new Wrapped {
    def value[T](given e: Exp[T]): T = e.neg(t.value[T])
  }
  def add(l: Wrapped, r: Wrapped) = new Wrapped {
    def value[T](given e: Exp[T]): T = e.add(l.value[T], r.value[T])
  }
}

def fix[A](f: (=> A) => A): A = f(fix(f))
def fromTree2[T: Exp](t: Tree): Either[ErrMsg, T] = fix(fromTreeExt[T] _)(t)

def fromTreeExt2[T]
  (recur: => (Tree => Either[ErrMsg, T]))
  (implicit e: Exp[T], m: Mult[T])
  : Tree => Either[ErrMsg, T] = {
    case Tree.Node("Mult", List(l , r)) =>
      for(lt <- recur(l).right; rt <- recur(r).right)
      yield m.mul(lt, rt)

    case t => fromTreeExt(recur).apply(t)
  
    }

given [T](given e: Exp[T]): Exp[NCtx => T] {
  def lit(i: Int): NCtx => T = {
    case NCtx.PosCtx => e.lit(i)
    case NCtx.NegCtx => e.neg(e.lit(i))
  }

  def neg(x: NCtx => T): NCtx => T = {
    case NCtx.PosCtx => x(NCtx.NegCtx)
    case NCtx.NegCtx => x(NCtx.PosCtx)
  }

  def add(l: NCtx => T, r: NCtx => T): NCtx => T =
    c => e.add(l(c), r(c))
}

given [T](given e: Mult[T]): Mult[NCtx => T] {
  def mul(l: NCtx => T, r: NCtx => T): NCtx => T = {
    case NCtx.PosCtx => e.mul(l(NCtx.PosCtx), r(NCtx.PosCtx))
    case NCtx.NegCtx => e.mul(l(NCtx.PosCtx), r(NCtx.NegCtx))
  }
}

type Ring[T] = (given Exp[T] , Mult[T]) => T
def tf1[T] (given Exp[T]) : T = add(lit(8), neg(add(lit(1), lit(2))))
def tfm1[T] : Ring[T] = add(lit(7), neg(mul(lit(1), lit(2))))
def tfm2[T] : Ring[T] = mul(lit(7), tf1)

def fromTree3[T: Exp : Mult](t: Tree): Either[ErrMsg, T] = fix(fromTreeExt2[T] _)(t)


@main def Expression() = { 
  val tf1Tree = tf1[Tree]
  println(tf1Tree)

  fromTree[Wrapped](tf1Tree) match {
    case Left(err) =>
      println(err)

    case Right(t) =>
      println(t.value[Int])
      println(t.value[String])
  }

  println(fromTree2[Int](tf1Tree))
  println(fromTree2[String](tf1Tree))

  val tfm2Tree = tfm2[Tree]
  println(tfm2Tree)
  println

  println(fromTree3[Int](tf1Tree))
  println(fromTree3[String](tf1Tree))
  println(fromTree3[Int](tfm2Tree))
  println(fromTree3[String](tfm2Tree))
  println
}
```

User Application of Tagless Final in dotty
---
```code scala
import scala.util.Random

object DataSource {

  /*enum AppError(msg: String) extends AnyRef {
    case UnknownError extends ErrorMsg(s"Unexpected Error")
    case UserNotFound(userId: UserId) extends ErrorMsg(s"User not found for id $userId")
    case UserNotProvided extends ErrorMsg(s"User id must be provided")
    case AlgorithmNotFound(recId: String) extends ErrorMsg(s"Algorithm not found for id $recId")
    case RecommendationsNotFound(userId: UserId, algo: String) extends ErrorMsg(s"Recommendations not found for $userId with algorithm '$algo'")
  }*/

  sealed trait AppError extends AnyRef {
    def message: String
  }
  case object UnknownError extends AppError {
    override def message: String = s"Unexpected Error"
  }
  case class UserNotFound(userId: UserId) extends AppError {
    override def message: String = s"User not found for id $userId"
  }
  case object UserNotProvided extends AppError {
    override def message: String = s"User id must be provided"
  }
  case class AlgorithmNotFound(recId: String) extends AppError {
    override def message: String = s"Algorithm not found for id $recId"
  }
  case class RecommendationsNotFound(userId: UserId, algo: String) extends AppError {
    override def message: String = s"Recommendations not found for $userId with algorithm '$algo'"
  }

  case class UserId(userId: Int) extends AnyVal
  case class Rec(recId: String, score: Float)
  case class UserRec(userId: UserId, recs: List[Rec])
  case class Result(algorithm: Algorithm, recs: UserRec)

  lazy val users = (1 until 10).map(UserId).toList
  lazy val recs = ('a' to 'z').map(c => Rec(c.toString, Random.nextFloat))

  lazy val recommendations = users.map {
    user => {
      if(user.userId % 2 == 0) {
        UserRec(user, List.empty)
      } else {
        UserRec(user, recs.toList)
      }
    }
  }

  def emptyRecs(user: Int): UserRec = {
    UserRec(UserId(user), List.empty)
  }

  case class Algorithm(name: String, run: UserId => Option[UserRec])

  val algo1 = Algorithm("algo1", userId => recommendations.find(u => u.userId == userId))
  val algo2 = Algorithm("algo2", userId => recommendations
    .find(u => u.userId == userId)
    .map(_.copy(recs = recs.filter(r => r.recId > "h").toList)))
  val algo3 = Algorithm("algo3" ,_ => None)

  lazy val algorithms = Map (
    "algo1" -> algo1,
    "algo2" -> algo2,
    "algo3" -> algo3
  )

  val algoDefault = Some("algo1")
  val limitDefault = 10
}

object algebras {
  import DataSource._

  trait Program[F[_]] {
    def (fa: F[A]) flatMap[A, B](afb: A => F[B]): F[B]
    def (fa: F[A]) map[A, B]    (ab: A => B): F[B]
    def (fa: F[A]) fold[A, B, C](first: B => C, second: A => C): C
  }

  trait UserRepo[F[_]] {
    def getUser(userId: Option[Int]): F[UserId]
  }
  def getUser[F[_]: UserRepo](userId: Option[Int]): F[UserId] = summon[UserRepo[F]].getUser(userId)

  trait Filter[F[_]] {
    def filter(userRec: UserRec, limit: Int): F[UserRec]
  }
  def filter[F[_]: Filter](userRec: UserRec, limit: Int): F[UserRec] = summon[Filter[F]].filter(userRec, limit)

  trait Limiter[F[_]] {
    def limit(limit: Option[Int]): F[Int]
  }
  def limiter[F[_]: Limiter](limit: Option[Int]): F[Int] = summon[Limiter[F]].limit(limit)

  trait AlgorithmRepo[F[_]] {
    def getAlgorithm(recommenderId: Option[String]): F[Algorithm]
    def execute(algo: Algorithm, userId: UserId): F[UserRec]
  }
  def getAlgorithm[F[_]: AlgorithmRepo](recommenderId: Option[String]): F[Algorithm] = summon[AlgorithmRepo[F]].getAlgorithm(recommenderId)
  def execute[F[_]: AlgorithmRepo](algo: Algorithm, userId: UserId): F[UserRec] = summon[AlgorithmRepo[F]].execute(algo, userId)
}

object interpreter {

  import DataSource._
  import algebras._

  type ETH[T] = Either[AppError,T]
  //type ETH[T] = Either[ErrorMsg,T]
  
  given UserRepo[Option] {
    def getUser(userId: Option[Int]): Option[UserId] =
      userId.filter(user => users.exists(_.userId == user)).map(UserId)
  }
  given UserRepo[ETH] {
    def getUser(userId: Option[Int]): ETH[UserId] = {
      for {
        userParam <- userId.map(UserId).toRight(UserNotProvided)
        userDb    <- users.find(_ == userParam).toRight(UserNotFound(userParam))
      } yield userDb
    }
  }
  
  given AlgorithmRepo[Option]{
    def getAlgorithm(recommenderId: Option[String]): Option[Algorithm] =
      recommenderId.orElse(algoDefault).flatMap(algorithms.get(_))

    def execute(algo: Algorithm, userId: UserId): Option[UserRec] = algo.run(userId)
  }
  given AlgorithmRepo[ETH]{
    def getAlgorithm(recommenderId: Option[String]): ETH[Algorithm] =
      recommenderId.orElse(algoDefault).flatMap(algorithms.get(_))
        .toRight(AlgorithmNotFound(recommenderId.getOrElse(algoDefault.get)))

    def execute(algo: Algorithm, userId: UserId): ETH[UserRec] =
      algo.run(userId).toRight(RecommendationsNotFound(userId, algo.name))
  }
  
  
  given Filter[Option] {
    def filter(userRec: UserRec, limit: Int): Option[UserRec] =
      Some(userRec.copy(recs = recs.slice(0, limit).toList))
  }
  given Filter[ETH] {
    def filter(userRec: UserRec, limit: Int): ETH[UserRec] = {
      Right(userRec.copy(recs = recs.slice(0, limit).toList))
    }
  }
  
  given Limiter[Option] {
    def limit(limit: Option[Int]): Option[Int] = limit.orElse(Some(limitDefault))
  }

    given Program[ETH] {
    def (fa: ETH[A]) flatMap[A, B](afb: A => ETH[B]): ETH[B] = fa.flatMap(afb)
    def (fa: ETH[A]) map[A, B](ab: A => B): ETH[B] = fa.map(ab)
    def (fa: ETH[A]) fold[A, B, C](first: B => C, second: A => C): C = fa.fold(error => first(error.asInstanceOf[B]), second(_))
  }

  given Program[Option] {
    def (fa: Option[A]) flatMap[A, B](afb: A => Option[B]): Option[B] = fa.flatMap(afb)
    def (fa: Option[A]) map[A, B](ab: A => B): Option[B] = fa.map(ab)
    def (fa: Option[A]) fold[A, B, C](first: B => C, second: A => C): C = fa.fold(first(UnknownError.asInstanceOf[B]))(second(_))
  }
}

/**
  * This is an exercise to explore advantages of moving from imperative design to FP design
  *
  * We are going to define program example which is going to take several arguments pased through command line
  * and evaluate each command line in order to execute diferent branch of the program.
  *
  * Story 1: As an user i want to get recommendations from an specific algorith, but if there are no recommendations for this algorith
  * or i forgot to specify what algorithm should be use i would like to have default recommendations from the best algorithm the system has.
  *
  * Story 2: As an user i want to get a message if recommendation's algorithm i requested is wrong.
  *
  * Story 3: As an user i want to be able to be retrieve with a limited number of recommendations
  *
  */
  object AppFunctional {

  import DataSource._
  import algebras._
  import interpreter.given

  def program(userId: Option[Int],
              recommenderId: Option[String] = None,
              limit: Option[Int] = None): Unit = {
    import interpreter._
    import interpreter.given

    val resultEither = getRecommendations[ETH](userId, recommenderId, limit)
    printResults[ETH](userId, resultEither)

    val resultOption = getRecommendations[Option](userId, recommenderId, limit)
    printResults(userId, resultOption)
  }

  def getRecommendations[F[_]](userId: Option[Int],recommenderId: Option[String],limit: Option[Int]) (given UserRepo[F], AlgorithmRepo[F], Filter[F], Program[F]): F[Result] = {
    for {
      user           <- getUser(userId)
      algorithm      <- getAlgorithm(recommenderId)
      result         <- execute(algorithm, user)
      limitFilter     = limit.getOrElse(limitDefault)
      resultFiltered <- filter(result, limitFilter)
    } yield Result(algorithm, resultFiltered)
  }


  def printResults[F[_]](userId: Option[Int], result: F[Result])(given Program[F]): Unit = {
    result.fold[Result, AppError, Unit](error => println(s"Error ${error}"), algoRes => {
      println(s"\nRecommnedations for userId ${algoRes.recs.userId}...")
      println(s"Algorithm ${algoRes.algorithm.name}")
      println(s"Recs: ${algoRes.recs.recs}")
    })
  }
}

@main def UserRecommandService = {
  import AppFunctional._

  program(Some(1), Some("algo1"), None)
  println("------------------------------\n")

  program(Some(2), Some("algo2"), Some(5))
  println("------------------------------\n")

  program(Some(3), Some("algo5"), Some(15))
  println("------------------------------\n")

  program(Some(14), Some("algo2"), Some(15))
  println("------------------------------\n")

  program(None, Some("algo3"), Some(15))
  println("------------------------------\n")

  program(Some(1), Some("algo3"), Some(15))
  println("------------------------------\n")
}
```
