package org.hablapps.meetup.funz.logic

import scalaz._, Scalaz._, Free._
import org.hablapps.meetup.common.logic.Domain._

object StoreProgram{

  def getUser(uid: Int): StoreProgram[User] = 
    liftF(GetUser(uid))

  def getGroup(gid: Int): StoreProgram[Group] = 
    liftF(GetGroup(gid))

  def putJoin(req: JoinRequest): StoreProgram[JoinRequest] = 
    liftF(PutJoin(req))

  def putMember(member: Member): StoreProgram[Member] = 
    liftF(PutMember(member))

}

object `package`{
  import StoreProgram._

  trait StoreInstruction[+_]
  case class GetUser(uid: Int) extends StoreInstruction[User]
  case class GetGroup(gid: Int) extends StoreInstruction[Group]
  case class PutJoin(join: JoinRequest) extends StoreInstruction[JoinRequest]
  case class PutMember(member: Member) extends StoreInstruction[Member]

  type StoreProgram[U] = Free[StoreInstruction, U]

  def Cond[X,Y](
    test: => Boolean,
    left: => StoreProgram[X], 
    right: => StoreProgram[Y]): StoreProgram[Either[X,Y]] = 
    if (test)
      left map (Left(_))
    else 
      right map (Right(_))

}

sealed class StoreError(val msg: String) extends RuntimeException

case class NonExistentEntity(id: Int) extends StoreError(s"Non-existent entity $id")
case class GenericError(override val msg: String) extends StoreError(msg)

  