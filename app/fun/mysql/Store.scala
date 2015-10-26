package org.hablapps.meetup.fun.mysql

import org.hablapps.meetup.fun.logic, logic._
import org.hablapps.meetup.common.logic.Domain._
import org.hablapps.meetup.common.mysql.Domain._

import play.api.db.slick.DB
import play.api.Play.current

import com.mysql.jdbc.exceptions.jdbc4.MySQLIntegrityConstraintViolationException
import scala.slick.driver.MySQLDriver.simple._


object Interpreter{

  def runInstruction[U](instruction: MeetupInstruction[U]): U =
    instruction match {
      
      case GetGroup(gid: Int) => 
        DB.withSession { implicit session =>
          group_table.byID(Some(gid)).first
        }
      
      case GetUser(uid: Int) =>
        DB.withSession { implicit session =>
          user_table.byID(Some(uid)).first
        }
    
      case PutJoin(join: JoinRequest) => 
        DB.withSession { implicit session =>
          val maybeId = join_table returning join_table.map(_.jid) += join
          join.copy(jid = maybeId)
        }

      case PutMember(member: Member) =>
        DB.withSession { implicit session =>
          val maybeId = member_table returning member_table.map(_.mid) += member
          member.copy(mid = maybeId)
        }

      case Inform(user, msg) =>
        println(s"Notifying organisers: $msg")
    }

  def run[U](store: MeetupProgram[U]): U = store match {
    case Return(value) => 
      value
    case Execute(instruction) => 
      runInstruction(instruction)
    case RunAndThen(program, next) => 
      run(next(run(program)))
  }

}
