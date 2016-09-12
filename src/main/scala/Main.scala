
import scalaz.{-\/, \/, \/-}
import shapeless.HList._
import shapeless.HList
import shapeless.HNil
import shapeless.::
import shapeless.BasisConstraint._
import shapeless.BasisConstraint

trait Inject[C, A] {
  def inj(a: A): C
  def prj(c: C): Option[A]
}

trait LowPriority0 {
  implicit def injectRightLeaf[A, B]: Inject[B \/ A, A] = new Inject[B \/ A, A] {
    def inj(a: A) = \/-(a)

    def prj(c: B \/ A): Option[A] = c.fold(_ => None, Some(_))
  }
}
trait LowPriority1 extends LowPriority0 {
  implicit def injectLeftLeaf[A, B]: Inject[A \/ B, A] = new Inject[A \/ B, A] {
    def inj(a: A) = -\/(a)
    def prj(c: A \/ B): Option[A] = c.fold(Some(_), _ => None)
  }
  implicit def injectLeft[A, L, R](implicit ev: Inject[L, A]): Inject[L \/ R, A] = new Inject[L \/ R, A] {
    def inj(a: A) = -\/(ev.inj(a))
    def prj(c: L \/ R): Option[A] = c.fold(ev.prj, _ => None)
  }
}

object Inject extends LowPriority1 {

  type In[A] = {
    type L[C] = Inject[C, A]
  }

  def apply[C, A](a: A)(implicit ev: Inject[C, A]): C = ev.inj(a)

  def lift[E, L, R](l: L \/ R)(implicit ev: Inject[E, L]): E \/ R = l.leftMap(ev.inj)

  implicit def injectEither[L, R, C](implicit ev1: Inject[C, L], ev2: Inject[C, R]): Inject[C, L \/ R] = new Inject[C, L \/ R] {
    def inj(a: L \/ R) = a match {
      case -\/(l) => ev1.inj(l)
      case \/-(r) => ev2.inj(r)
    }
    def prj(c: C): Option[L \/ R] = {
      ev1.prj(c).map(-\/(_))
        .orElse(ev2.prj(c).map(\/-(_)))
    }
  }
}

case class ErrorA()
case class ErrorB()
case class ErrorC()
case class ErrorD()

trait Split[C0, C1, A] {
  def split(c0: C0): C1 \/ A
}

object Split {
  implicit def eliminateLeftLeaf[L, R]: Split[L \/ R, R, L] = new Split[L \/ R, R, L] {
    override def split(c0: \/[L, R]): R \/ L = ~c0
  }

  implicit def eliminateRightLeaf[L, R]: Split[L \/ R, L, R] = new Split[L \/ R, L, R] {
    override def split(c0: \/[L, R]): L \/ R = c0
  }

  implicit def eliminateLeft[L, R, L0, A](implicit e: Split[L, L0, A]): Split[L \/ R, L0 \/ R, A] = new Split[L \/ R, L0 \/ R, A] {
    override def split(c0: \/[L, R]): \/[\/[L0, R], A] = c0 match {
      case -\/(l) => e.split(l) match {
        case \/-(a) => \/-(a)
        case -\/(l0) => -\/(-\/(l0))
      }
      case \/-(r) => -\/(\/-(r))
    }
  }

  implicit def eliminateRight[L, R, R0, A](implicit e: Split[R, R0, A]): Split[L \/ R, L \/ R0, A] = new Split[L \/ R, L \/ R0, A] {
    override def split(c0: \/[L, R]): \/[\/[L, R0], A] = c0 match {
      case -\/(l) => -\/(-\/(l))
      case \/-(r) => e.split(r) match {
        case -\/(r0) => -\/(\/-(r0))
        case \/-(a) => \/-(a)
      }
    }
  }
}

object Main {
  import Inject.In

  def methodA(input: Int): ErrorA \/ Int = ???
  def methodB(input: Int): ErrorB \/ Int = ???

  def eliminate[E0, E](fn: Int => E0 \/ Int)(implicit split: Split[E0, E, ErrorA]): E \/ Int = {
    fn(0) match {
      case \/-(i) => \/-(i)
      case -\/(e) => {
        split.split(e) match {
          case \/-(errora) => \/-(0)
          case otherError @ -\/(_) => otherError
        }
      }
    }
  }

  def genericMethodA[E](input: Int)(implicit ev: Inject[E, ErrorC]): E \/ Int = {
    -\/(Inject(ErrorC()))
  }

  def composition[E : In[ErrorA]#L : In[ErrorB]#L](input: Int, f1: Int => ErrorA \/ Int, f2: Int => ErrorB \/ Int): E \/ Int = {
    if (input % 2 == 0) {
      Inject.lift(f1(input))
    } else {
      Inject.lift(f2(input))
    }
  }

  def composeFurther[E : In[ErrorA]#L : In[ErrorB]#L : In[ErrorC]#L](input: Int, f1: Int => ErrorA \/ Int, f2: Int => ErrorB \/ Int): E \/ Int = {
    Inject.lift(f1(input))
      .flatMap(i => Inject.lift(f2(i)))
      .flatMap(i => -\/(Inject(ErrorC())))
      .flatMap(i => composition(i, f1, f2))
  }

  type ABC = ErrorA \/ ErrorB \/ ErrorC
  type BCD = ErrorB \/ ErrorC \/ ErrorD
  type ABCD = ErrorA \/ ErrorB \/ ErrorC \/ ErrorD

  def composeAgain[E : In[ABC]#L : In[BCD]#L](input: Int, f1: Int => ABC \/ Int, f2: Int => BCD \/ Int): E \/ Int = {
    for {
      a <- Inject.lift(f1(input))
      b <- Inject.lift(f2(a))
    } yield a + b
  }

  def main(args: Array[String]): Unit = {

    implicitly[Split[ErrorA \/ ErrorB \/ ErrorC, ErrorB \/ ErrorC, ErrorA]]

    val i = implicitly[Inject[ErrorA \/ ErrorB \/ ErrorC \/ ErrorD, ErrorC \/ ErrorA \/ ErrorB]]

    val isi: Int \/ String = Inject[Int \/ String, Int](1)
    val iss: Int \/ String = Inject[Int \/ String, String]("1")

    composition[ErrorA \/ ErrorB](1, methodA, methodB)
    composeFurther[ErrorA \/ ErrorB \/ ErrorC](1, methodA, methodB)

    composeAgain[ABCD](0, ???, ???)
  }

}
