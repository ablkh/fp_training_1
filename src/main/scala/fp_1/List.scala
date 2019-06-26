package fp_1

sealed trait List[+A]{
  def tail:List[A]
  def drop(n:Int):List[A]
  def init:List[A]
  def sum(ints: List[Int]):Int = ints.foldRight(0) (_+_)
  def product(ints: List[Int]):Int = ints.foldRight(1) (_*_)
  def sum2(ints: List[Int]):Int = ints.foldLeft(0) (_+_)
  def product2(ints: List[Int]):Int = ints.foldLeft(1) (_*_)
  def foldRight[B](z: B)(f: (A, B) => B): B
  def length:Int
  def length2:Int
  def foldLeft[B](z: B)(f: (B, A) => B): B
  def addOne(list: List[Int]): List[Int]
  def tostring(list: List[_]): List[String]
  def map[B](f: (A=>B)):List[B]
  def filter(p: (A=>Boolean)):List[A]
  def append[A](list1:List[A],list2:List[A]):List[A] = list1 match {
    case Nil => list2
    case Cons(e, t) => Cons(e, append(t, list2))
  }

  def flatMap[B](f: (A => List[B])): List[B]
  def fproduct[B](f: (A => B)):List[(A,B)] = map(e => {(e, f (e))})

}

object List {

  val foncteur = new Foncteur[List]{
    def map[A,B](fa: List[A])(f: (A => B)):List[B] = fa.map(f)
  }

  val foncteurApplicatif = new Applicative[List]{
    def point[A](a: A): List[A] = Cons(a, Nil)

    def ap[A, B](fa: List[A])(f: List[A => B]): List[B] = f.flatMap(fa.map)

  }
}

case object Nil extends List[Nothing]{

  def tail:List[Nothing] = Nil
  def drop(n : Int):List[Nothing] = Nil
  def init:List[Nothing] = Nil
  def foldRight[B](z: B)(f: (Nothing, B) => B): B = z
  def length:Int = 0
  def foldLeft[B](z: B)(f: (B, Nothing) => B): B = z
  def length2:Int = 0
  def addOne(list: List[Int]): List[Int] = Nil
  def tostring(list: List[_]): List[String] = Nil
  def map[B](f: (Nothing=>B)):List[B] = Nil
  def filter(p: (Nothing=>Boolean)):List[Nothing] = Nil
  def flatMap[B](f: (Nothing => List[B])): List[B] = Nil



  def main(args : Array[String]): Unit ={
    val list = Cons(1,Cons(2,Nil))
    val r = list.flatMap(x => Cons(0,Cons(x,Nil)))
    val result_of_ap = List.foncteurApplicatif.ap(Cons(1,Cons(2,Cons(3, Nil))))(Cons(
      (i: Int) => i + 1,
      Cons((i: Int) => i - 1, Nil)
    ))

    val result_of_ap2 = List.foncteurApplicatif.ap2(Cons(1,Cons(2,Nil)), Cons(1,Cons(2,Nil)))(Cons(
      (i: Int, i2:Int) => i + i2,
      Cons((i: Int, i2:Int) => i * i2, Nil)
    ))
    println(result_of_ap2)
  }
}

case class Cons[+A](elt : A, next : List[A]) extends List[A]{

  def tail:List[A] = next

  def drop(n : Int):List[A] = n match {
    case 0 => this
    case _ => tail.drop(n-1)
  }

  def init : List[A] = this match {
    case Cons(x, Nil) => this
    case _ => Cons(elt, tail.init)
  }

  def foldRight[B](z: B)(f: (A, B) => B): B = f(elt, tail.foldRight(z)(f))
  def length:Int = foldRight(1)((_, b:Int) => 1 + b)
  def foldLeft[B](z: B)(f: (B, A) => B): B = tail.foldLeft(f(z, elt))(f)
  def length2:Int = foldLeft(1)((b:Int, _) => 1 + b)
  def addOne(list: List[Int]): List[Int] = list.map(_ + 1)
  def tostring(list: List[_]): List[String] = list.map(_.toString)
  def map[B](f: (A=>B)):List[B] = Cons(f(elt), tail.map(f))
  def filter(p: (A=>Boolean)):List[A] = if( p (elt)){ tail.filter(p)}else{ Cons(elt, tail.filter(p))}
  def flatMap[B](f: (A => List[B])): List[B] = map(f) match {
    case Cons(e, t) => t.foldLeft(e)(append)
    case Nil => Nil
  }



}
