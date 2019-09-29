## Revisiting Tagless Final Interpreters in Scala Dotty (0.19.0-RC1)
[original link] https://gist.github.com/OlivierBlanvillain/48bb5c66dbb0557da50465809564ee80

Just try to re-write the original code in scala dotty.
—

```scala
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


@main def test() = { 
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
