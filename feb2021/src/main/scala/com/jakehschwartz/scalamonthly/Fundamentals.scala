package com.jakehschwartz.scalamonthly

import java.time.Instant
import scala.collection.immutable.Nil
import scala.annotation.tailrec

object Fundamentals {

  /**
   * Sum the list of integers `l`
   */
  def one(l: List[Int]): Int = l.foldRight(0)(_ + _)


  /**
   * Concatenate the list of chars into a String
   */
  def two(l: List[Char]): String = l.foldRight("")(_ + _)

  /**
   * Stringify the optional input
   * If the option is None, return "None" as a string.
   * If the option is defined, return the value it contains as a string wrapped in "Some(...)"
   */
  def three(l: Option[String]): String = l.fold("None")(x => s"Some($x)")

  sealed abstract class JobStatus extends Product with Serializable {
    import JobStatus._
    def fold[A](stopped: => A)(running: JobStatus.Running => A): A = this match {
      case r: Running => running(r)
      case Stopped => stopped
    }
  }
  object JobStatus {
    final case class Running(startedAt: Instant) extends JobStatus
    case object Stopped extends JobStatus
  }
  /**
   * Implement the fold operation for JobStatus above. Use it to return "Stopped" for JobStatus.Stopped job
   * and "Started at $startedAt" for JobStatus.Running.
   */
  def four(l: JobStatus): String = l.fold("Stopped")(r => s"Started at ${r.startedAt}")

  /**
   * Return the length of the input list `l`
   */
  def five[A](l: List[A]): Int = l.foldRight(0)((_, len) => len + 1)

  /**
   * Implement the contains function where true is returned if `i` is contained inside of `l`
   * otherwise false is returned
   */
  def six[A](l: List[A], i: A): Boolean = l.foldRight(false)((x, r) => x == i || r)

  /**
   * Reverse the input list `l`
   */
  def seven[A](l: List[A]): List[A] = l.foldLeft(List.empty[A])((r, e) => e +: r)
  sealed abstract class MyList[+A] extends Product with Serializable
  object MyList {
    case object Empty extends MyList[Nothing]
    final case class Cons[A](h: A, t: MyList[A]) extends MyList[A]
  }
  /**
   * Transform the input list `l` into the equivalent `MyList`
   */
  def eight[A](l: List[A]): MyList[A] = l.foldRight[MyList[A]](MyList.Empty)((e, r) => MyList.Cons(e, r))

  /**
   * Implement a tail-recursive foldLeft function for the List type
   */
  @tailrec
  def foldLeft[A, B](l: List[A])(base: B)(f: (B, A) => B): B = if (l.isEmpty) {
    base
  } else {
    foldLeft(l.tail)(f(base, l.head))(f)
  }

  /**
   * Implement the foldRight function for the list type. Do NOT use the reverse operation
   * on List as part of your implementation.
   */
  @tailrec
  def foldRight[A, B](l: List[A])(base: B)(f: (A, B) => B): B = if (l.isEmpty) {
    base
  } else {
    foldRight(l.dropRight(1))(f(l.last, base))(f)
  }


}