package fp_1

trait Option[+A] {
  def map[B](f: A => B): Option[B]
  def flatMap[B](f: A => Option[B]): Option[B]
  def getOrElse[B >: A](default: B): B
  def orElse[B >: A](ob: Option[B]): Option[B]
  def filter(f: A => Boolean): Option[A]
  def fproduct[B](f: (A => B)):Option[(A,B)] = map(e => (e, f (e)))
}

case object None extends Option[Nothing]{
  def map[B](f: Nothing => B): Option[B] = None
  def flatMap[B](f: Nothing => Option[B]): Option[B] = None
  def getOrElse[B >: Nothing](default: B): B = default
  def orElse[B >: Nothing](ob: Option[B]): Option[B] = ob
  def filter(f: Nothing => Boolean): Option[Nothing] = None

  def main(args : Array[String])={
    val o = Some(1)
    val r = o.flatMap(i => Some(i + 1))
    println(r)
  }
}

case class Some[+A](elt : A) extends Option[A]{
  def map[B](f: A => B): Option[B] = Some(f (elt))
  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)
  def getOrElse[B >: A](default: B): B = elt
  def orElse[B >: A](ob: Option[B]): Option[B] = this
  def filter(f: A => Boolean): Option[A] = if(!f (elt)){this}else{None}
}
