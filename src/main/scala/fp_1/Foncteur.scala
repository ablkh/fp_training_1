package fp_1

trait Foncteur[F[_]] {

  def map[A,B](e : F[A])(f: A => B) : F[B]
  def fproduct[A,B](e : F[A])(f : A => B):  F[(A, B)] = map(e)(a => (a, f(a)))
  def fpair[A, B](fa: F[A]): F[(A, A)] = map(fa)(a => (a, a))
  def mapply[A, B](a : A)(f : F[A => B]) : F[B] = map(f)(j => j(a))
  def lift[A, B](f : A => B) : F[A] => F[B] = (fa:F[A]) => map(fa)(f)

}


trait Applicative[F[_]] extends Foncteur[F]{

  def point[A](a: A): F[A]

  def ap[A, B](fa: F[A])(f: F[A => B]): F[B]

  override def map[A, B](fa: F[A])(f: (A => B)): F[B] = ap(fa)(point(f))



  def ap2[A, B, C](fa: F[A], fb: F[B])(f: F[(A, B) => C]): F[C] = {
    val curriedF: F[A => B => C] = map(f)(_.curried)

    val fAppliedOnFA: F[(B) => C] = ap(fa)(curriedF)

    ap(fb)(fAppliedOnFA)
  }

  def apply2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = ap2(fa, fb)(point(f))

  def mapply2[A, B, C](a: A, b: B)(f: F[(A, B) => C]): F[C] = ap2(point(a), point(b)) (f)

}