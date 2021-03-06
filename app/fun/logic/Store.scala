package org.hablapps.meetup.fun.logic

import org.hablapps.meetup.common.logic.Domain._

object Store{
  
  def getGroup(id: Int): Store[Group] = 
    StoreAndThen(GetGroup(id), (t: Group) => Return(t))
  
  def putGroup(group: Group): Store[Group] = 
    StoreAndThen(PutGroup(group), (t: Group) => Return(t))
  
  def getUser(id: Int): Store[User] =  
    StoreAndThen(GetUser(id), (t: User) => Return(t))

  def putUser(user: User): Store[User] =  
    StoreAndThen(PutUser(user), (t: User) => Return(t))

  def putJoin(t: JoinRequest): Store[JoinRequest] = 
    StoreAndThen(PutJoin(t), (t1: JoinRequest) => Return(t1))

  def putMember(t: Member): Store[Member] = 
    StoreAndThen(PutMember(t), (t1: Member) => Return(t1))

  def Cond[X,Y](
    test: => Boolean,
    left: => Store[X], 
    right: => Store[Y]): Store[Either[X,Y]] = 
    if (test)
      left map (Left(_))
    else 
      right map (Right(_))

  trait Interpreter{
    def runFrom[A, B](initial: Store[A])(
      program: A => Store[B]): Either[StoreError,B] = 
      run(initial flatMap program)

    def run[A](program: Store[A]): Either[StoreError, A]
  }

}

trait StoreInstruction[_]
case class GetUser(uid: Int) extends StoreInstruction[User]
case class GetGroup(gid: Int) extends StoreInstruction[Group]
case class PutUser(user: User) extends StoreInstruction[User]
case class PutGroup(grupo: Group) extends StoreInstruction[Group]
case class PutJoin(join: JoinRequest) extends StoreInstruction[JoinRequest]
case class PutMember(member: Member) extends StoreInstruction[Member]
case class Fail(exception: Exception) extends StoreInstruction[Nothing]

trait Store[U]{
  
  def flatMap[V](f: U => Store[V]): Store[V] = this match {
    case StoreAndThen(inst, next) => 
      StoreAndThen(inst, next andThen (_ flatMap f))
    case Return(t) => 
      f(t)
  }

  def map[V](f: U => V): Store[V] = 
    this flatMap ( u => Return(f(u)) )

}

case class Return[U](value: U) extends Store[U]
case class StoreAndThen[U,V](inst: StoreInstruction[U], next: U => Store[V])
  extends Store[V]  

sealed class StoreError(val msg: String) extends Exception(msg)

case class NonExistentEntity(id: Int) extends StoreError(s"Non-existent entity $id")
case class GenericError(override val msg: String) extends StoreError(msg)
case class WrapError(exception: Exception) extends StoreError(exception.getMessage)

