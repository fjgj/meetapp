package org.hablapps.meetup.oo.logic

import scala.concurrent.Future

import org.hablapps.meetup.common.logic.Domain._

trait Store{

  def getGroup(gid: Int): Future[Group]
  

  def getUser(uid: Int): Future[User]
  

  def putJoin(join: JoinRequest): Future[JoinRequest]
  

  def putMember(member: Member): Future[Member]

}

sealed class StoreError(val msg: String) extends RuntimeException

case class NonExistentEntity(id: Int) extends StoreError(s"Non-existent entity $id")
case class GenericError(override val msg: String) extends StoreError(msg)

