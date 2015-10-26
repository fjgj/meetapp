package org.hablapps.meetup.fun.logic

import org.hablapps.meetup.common.logic.Domain._

object MeetupProgram{
  import Monad._
  
  implicit val MeetupProgramMonad = new Monad[MeetupProgram]{
    def returns[X](value: X): MeetupProgram[X] = 
      Return(value)

    def flatMap[U, V](p: MeetupProgram[U])(f: U => MeetupProgram[V]): MeetupProgram[V] = 
      RunAndThen(p, f)
  }

  implicit val MeetupProgramStoreEff = new StoreEff[MeetupProgram]{
    def getGroup(id: Int): MeetupProgram[Group] = 
      Execute(GetGroup(id))
    
    def putGroup(group: Group): MeetupProgram[Group] = 
      Execute(PutGroup(group))
    
    def getUser(id: Int): MeetupProgram[User] =  
      Execute(GetUser(id))

    def putUser(user: User): MeetupProgram[User] =  
      Execute(PutUser(user))

    def putJoin(t: JoinRequest): MeetupProgram[JoinRequest] = 
      Execute(PutJoin(t))

    def putMember(t: Member): MeetupProgram[Member] = 
      Execute(PutMember(t))
  }

  implicit val MeetupProgramMessagingEff = new MessagingEff[MeetupProgram]{
    def inform(user: Int, msg: String): MeetupProgram[Unit] = 
      Execute(Inform(user, msg))
  }
  
  def Cond[P[_]: Monad, X,Y](
    test: => Boolean,
    left: => P[X], 
    right: => P[Y]): P[Either[X,Y]] = 
    if (test)
      left map (Left(_))
    else 
      right map (Right(_))

  // def left[X,Y](program: MeetupProgram[X]): MeetupProgram[Either[X,Y]] = 
  //   program map (x => Left[X,Y](x))

  // def right[X,Y](program: MeetupProgram[Y]): MeetupProgram[Either[X,Y]] = 
  //   program map (y => Right[X,Y](y))

  // trait Interpreter{
  //   def runFrom[A, B](initial: MeetupProgram[A])(
  //     program: A => MeetupProgram[B]): Either[StoreError,B] = 
  //     run(initial flatMap program)

  //   def run[A](program: MeetupProgram[A]): Either[StoreError, A]
  // }

}

trait MeetupInstruction[_]
case class GetUser(uid: Int) extends MeetupInstruction[User]
case class GetGroup(gid: Int) extends MeetupInstruction[Group]
case class PutUser(user: User) extends MeetupInstruction[User]
case class PutGroup(grupo: Group) extends MeetupInstruction[Group]
case class PutJoin(join: JoinRequest) extends MeetupInstruction[JoinRequest]
case class PutMember(member: Member) extends MeetupInstruction[Member]
case class Fail(exception: Exception) extends MeetupInstruction[Nothing]
case class Inform(user: Int, msg: String) extends MeetupInstruction[Unit]

sealed trait MeetupProgram[U]
case class Return[U](value: U) extends MeetupProgram[U]
case class Execute[U](inst: MeetupInstruction[U]) extends MeetupProgram[U]  
case class RunAndThen[U,V](program: MeetupProgram[U], next: U => MeetupProgram[V]) extends MeetupProgram[V]  

sealed class StoreError(val msg: String) extends Exception(msg)

case class NonExistentEntity(id: Int) extends StoreError(s"Non-existent entity $id")
case class GenericError(override val msg: String) extends StoreError(msg)
case class WrapError(exception: Exception) extends StoreError(exception.getMessage)

