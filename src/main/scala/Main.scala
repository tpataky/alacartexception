
import scalaz.{-\/, \/, \/-}
import shapeless.HList._
import shapeless.HList
import shapeless.HNil
import shapeless.::
import shapeless.BasisConstraint._
import shapeless.BasisConstraint

trait Inject[C, A] {
  def inj(a: A): C
}

trait LowPriority {
  implicit def injectRightLeaf[A, B]: Inject[B \/ A, A] = new Inject[B \/ A, A] {
    def inj(a: A) = \/-(a)
  }
  implicit def injectLeftLeaf[A, B]: Inject[A \/ B, A] = new Inject[A \/ B, A] {
    def inj(a: A) = -\/(a)
  }
  implicit def injectLeft[A, L, R](implicit ev: Inject[L, A]): Inject[L \/ R, A] = new Inject[L \/ R, A] {
    def inj(a: A) = -\/(ev.inj(a))
  }
}

object Inject extends LowPriority {

  type In[A] = {
    type L[C] = Inject[C, A]
  }

  def apply[C, A](a: A)(implicit ev: Inject[C, A]): C = ev.inj(a)

  def liftLeft[E, L, R](l: L \/ R)(implicit ev: Inject[E, L]): E \/ R = l.leftMap(ev.inj)

  implicit def injectEither[L, R, C](implicit ev1: Inject[C, L], ev2: Inject[C, R]): Inject[C, L \/ R] = new Inject[C, L \/ R] {
    def inj(a: L \/ R) = a match {
      case -\/(l) => ev1.inj(l)
      case \/-(r) => ev2.inj(r)
    }
  }
}

case class ErrorA()
case class ErrorB()
case class ErrorC()
case class ErrorD()

trait True
trait False

object Main {
  import Inject.In

  def methodA(input: Int): ErrorA \/ Int = ???
  def methodB(input: Int): ErrorB \/ Int = ???

  def genericMethodA[E](input: Int)(implicit ev: Inject[E, ErrorC]): E \/ Int = {
    -\/(Inject(ErrorC()))
  }

  def composition[E : In[ErrorA]#L : In[ErrorB]#L](input: Int, f1: Int => ErrorA \/ Int, f2: Int => ErrorB \/ Int): E \/ Int = {
    if (input % 2 == 0) {
      Inject.liftLeft(f1(input))
    } else {
      Inject.liftLeft(f2(input))
    }
  }

  def composeFurther[E : In[ErrorA]#L : In[ErrorB]#L : In[ErrorC]#L](input: Int, f1: Int => ErrorA \/ Int, f2: Int => ErrorB \/ Int): E \/ Int = {
    Inject.liftLeft(f1(input))
      .flatMap(i => Inject.liftLeft(f2(i)))
      .flatMap(i => -\/(Inject[E, ErrorC](ErrorC())))
      .flatMap(i => composition(i, f1, f2))
  }

  type ABC = ErrorA \/ ErrorB \/ ErrorC
  type BCD = ErrorB \/ ErrorC \/ ErrorD

  def composeAgain[E : In[ABC]#L : In[BCD]#L](input: Int, f1: Int => ABC \/ Int, f2: Int => BCD \/ Int): E \/ Int = {
    for {
      a <- Inject.liftLeft(f1(input))
      b <- Inject.liftLeft(f2(a))
    } yield a + b
  }

  def main(args: Array[String]): Unit = {
    val isi: Int \/ String = Inject[Int \/ String, Int](1)
    val iss: Int \/ String = Inject[Int \/ String, String]("1")

    composition[ErrorA \/ ErrorB](1, methodA, methodB)
    composeFurther[ErrorA \/ ErrorB \/ ErrorC](1, methodA, methodB)

    composeAgain[ErrorB \/ ErrorC \/ ErrorA \/ ErrorD](0, ???, ???)
  }

}
