package org.hablapps.meetup.fun.logic

import org.hablapps.meetup.common.logic.Domain._

object StoreProgram{
  
  def getGroup(id: Int): StoreProgram[Group] = 
    Execute(GetGroup(id))
  
  def putGroup(group: Group): StoreProgram[Group] = 
    Execute(PutGroup(group))
  
  def getUser(id: Int): StoreProgram[User] =  
    Execute(GetUser(id))

  def putUser(user: User): StoreProgram[User] =  
    Execute(PutUser(user))

  def putJoin(t: JoinRequest): StoreProgram[JoinRequest] = 
    Execute(PutJoin(t))

  def putMember(t: Member): StoreProgram[Member] = 
    Execute(PutMember(t))

  def Cond[X,Y](
    test: => Boolean,
    left: => StoreProgram[X], 
    right: => StoreProgram[Y]): StoreProgram[Either[X,Y]] = 
    if (test)
      left map (Left(_))
    else 
      right map (Right(_))

  def left[X,Y](program: StoreProgram[X]): StoreProgram[Either[X,Y]] = 
    program map (x => Left[X,Y](x))

  def right[X,Y](program: StoreProgram[Y]): StoreProgram[Either[X,Y]] = 
    program map (y => Right[X,Y](y))

  def returns[X](value: X): StoreProgram[X] = 
    Return(value)

  trait Interpreter{
    def runFrom[A, B](initial: StoreProgram[A])(
      program: A => StoreProgram[B]): Either[StoreError,B] = 
      run(initial flatMap program)

    def run[A](program: StoreProgram[A]): Either[StoreError, A]
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

trait StoreProgram[U]{
  
  def flatMap[V](f: U => StoreProgram[V]): StoreProgram[V] = 
    RunAndThen(this, f)

  def map[V](f: U => V): StoreProgram[V] = 
    this flatMap ( u => Return(f(u)) )

}

case class Return[U](value: U) extends StoreProgram[U]
case class Execute[U](inst: StoreInstruction[U]) extends StoreProgram[U]  
case class RunAndThen[U,V](program: StoreProgram[U], next: U => StoreProgram[V]) extends StoreProgram[V]  

sealed class StoreError(val msg: String) extends Exception(msg)

case class NonExistentEntity(id: Int) extends StoreError(s"Non-existent entity $id")
case class GenericError(override val msg: String) extends StoreError(msg)
case class WrapError(exception: Exception) extends StoreError(exception.getMessage)

