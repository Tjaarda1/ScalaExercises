package io.github.divios

import scala.annotation.tailrec

@main
def main(): Unit = {
  val a = List.drop(List(1, 2, 4, 5, 2, 5, 10), 2)
  println(List.mkString(a, ", "))
}

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def empty[A]: List[A] = Nil

  def fill[A](a: A)(n: Int): List[A] = {
    @tailrec
    def go(a: A, n: Int, l: List[A]): List[A] =
      if (n <= 0) l else go(a, n - 1, Cons(a, l))

    go(a, n, Nil)
  }

  def apply[A](a: A*): List[A] = {
    @tailrec
    def go(a: Seq[A], l: List[A]): List[A] =
      if (a.isEmpty) l else go(a.dropRight(1), Cons(a.last, l))

    go(a, Nil)
  }

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = l match
    case Nil => l
    case Cons(head, tail) => if (n > 0) drop(tail, n - 1) else l

  def mkString(list: List[?], space: String): String =
    foldLeft(list, "")(_ + space + _) substring space.length

  def length(list: List[?]): Int =
    foldLeft(list, 0)((a, b) => a + 1)

  def sum(ints: List[Int]): Int =
    foldLeft(ints, 0)(_ + _)

  def product(doubles: List[Int]): Int =
    foldLeft(doubles, 1)(_ * _)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List.empty)((a, b) => Cons(b, a))

  def append[A](l: List[A], a: A): List[A] = Cons(a, l)

  @tailrec
  def foldLeft[A, B](list: List[A], start: B)(f: (B, A) => B): B =
    list match
      case Nil => start
      case Cons(head, tail) => foldLeft(tail, f(start, head))(f)

}