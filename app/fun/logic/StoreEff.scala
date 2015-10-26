package org.hablapps.meetup.fun.logic

import org.hablapps.meetup.common.logic.Domain._

trait Monad[Program[_]]{
  def returns[A](value: A): Program[A]
  def flatMap[A,B](ma: Program[A])(f: A => Program[B]): Program[B]
}

object Monad{
  def apply[Program[_]: Monad] = implicitly[Monad[Program]]

  implicit class MonadOps[P[_]: Monad, A](p: P[A]){
    def flatMap[B](f: A => P[B]): P[B] = Monad[P].flatMap(p)(f)
    def map[B](f: A => B): P[B] = Monad[P].flatMap(p)(f andThen Monad[P].returns)
  }
}

abstract class StoreEff[Program[_]: Monad]{
  def getGroup(id: Int): Program[Group] 
  def putGroup(group: Group): Program[Group] 
  def getUser(id: Int): Program[User] 
  def putUser(user: User): Program[User]  
  def putJoin(t: JoinRequest): Program[JoinRequest] 
  def putMember(t: Member): Program[Member] 
}

object StoreEff{
  def apply[Program[_]: StoreEff]: StoreEff[Program] = implicitly[StoreEff[Program]]
}

abstract class MessagingEff[Program[_]](implicit val monad: Monad[Program]){
  def inform(user: Int, msg: String): Program[Unit]
}

object MessagingEff{
  def apply[Program[_]: MessagingEff] = implicitly[MessagingEff[Program]]
}