package fpinscala.monoids

import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps // infix syntax for `Par.map`, `Par.flatMap`, etc
import language.higherKinds

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2
    val zero = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a: Int, b: Int) = a * b
    val zero = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a: Boolean, b: Boolean) = a || b
    val zero = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(x: Boolean, y: Boolean) = x && y
    val zero = true
  }

  // Relevant comment from the solutions:
  //
  // Notice that we have a choice in how we implement `op`.
  // We can compose the options in either order. Both of those implementations
  // satisfy the monoid laws, but they are not equivalent.
  // This is true in general--that is, every monoid has a _dual_ where the
  // `op` combines things in the opposite order. Monoids like `booleanOr` and
  // `intAddition` are equivalent to their duals because their `op` is commutative
  // as well as associative.

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    // dual version being y orElse x
    def op(x: Option[A], y: Option[A]): Option[A] = x orElse y
    def zero = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(f: A => A, g: A => A) = f andThen g // dual: f compose g
    def zero = (a: A) => a
  }

  // TODO: Placeholder for `Prop`. Remove once you have implemented the `Prop`
  // data type from Part 2.
  trait Prop {}

  // TODO: Placeholder for `Gen`. Remove once you have implemented the `Gen`
  // data type from Part 2.

  import fpinscala.testing._
  import Prop._
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = ???

  def trimMonoid(s: String): Monoid[String] = ???

  def concatenate[A](as: List[A], m: Monoid[A]): A = {
    as.foldLeft(m.zero)(m.op)
    // or to spell this out:
    as.foldLeft(m.zero)((acc, a) => m.op(acc, a))
  }

  // converting the whole list to Bs first
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = {
    as.map(f).foldLeft(m.zero)(m.op)
  }

  // or without even using map:
  def foldMap2[A, B](as: List[A], m: Monoid[B])(f: A => B): B = {
    as.foldLeft(m.zero)((acc, a) => m.op(acc, f(a))) // acc: B
  }

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    ???

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    ???

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    ???

  def ordered(ints: IndexedSeq[Int]): Boolean =
    ???

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  def par[A](m: Monoid[A]): Monoid[Par[A]] = 
    ???

  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = 
    ???

  val wcMonoid: Monoid[WC] = ???

  def count(s: String): Int = ???

  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    ???

  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] =
    ???

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] =
    ???

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    ???
}

trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    ???

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    ???

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    ???

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    ???

  def toList[A](as: F[A]): List[A] =
    ???
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    ???
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    ???
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    ???
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    ???
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    ???
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    ???
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    ???
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    ???
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
    ???
  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) =
    ???
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) =
    ???
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    ???
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) =
    ???
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =
    ???
}

