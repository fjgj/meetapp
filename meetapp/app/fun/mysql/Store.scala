package org.hablapps.meetup.fun.mysql

import scala.concurrent.{Await, Future, duration, ExecutionContext}
import ExecutionContext.Implicits.global
import duration._

import org.hablapps.meetup.fun.logic, logic._
import org.hablapps.meetup.common.logic.Domain._
import org.hablapps.meetup.common.mysql.Domain._

object Interpreter{
  import dbConfig._, driver.api._

  def runInstruction[U](instruction: StoreInstruction[U]): Future[U] =
    instruction match {
      
      case GetGroup(gid: Int) => 
        db.run(group_table.byID(Some(gid)).result.head)
      
      case GetUser(uid: Int) =>
        db.run(user_table.byID(Some(uid)).result.head)

      case PutJoin(join: JoinRequest) => 
        db.run((join_table returning join_table.map(_.jid)
                into ((req,id) => req.copy(jid = id))) += join)

      case PutMember(member: Member) =>
        db.run((member_table returning member_table.map(_.mid)
                into ((mem,id) => mem.copy(mid = id))) += member)

    }

  def run[U](store: Store[U]): Future[U] = store match {
    case Return(value) => 
      Future(value)
    case StoreAndThen(instruction, next) => 
      for {
        result <- runInstruction(instruction)
        r <- run(next(result))
      } yield r
  }

}
