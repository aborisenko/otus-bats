package me.chuwy.otusbats

trait Monad[F[_]] extends Functor[F] { self =>
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def point[A](a: A): F[A]

  def flatten[A](fa: F[F[A]]): F[A] = self.flatMap(fa)(a => a)
}

object Monad {
  implicit def optionMonad: Monad[Option] = new Monad[Option] {
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
    override def point[A](a: A): Option[A] = Option(a)
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

  implicit def listMonad: Monad[List] = new Monad[List] {
    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
    override def point[A](a: A): List[A] = List(a)
    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }

  type EitherT[A] = Either[Any, A]
  implicit def eitherMonad: Monad[EitherT] = new Monad[EitherT] {
    override def flatMap[A, B](fa: EitherT[A])(f: A => EitherT[B]): EitherT[B] = fa.flatMap(f)

    override def point[A](a: A): EitherT[A] = Right(a)

    override def map[A, B](fa: EitherT[A])(f: A => B): EitherT[B] = fa.map(f)
  }
}
