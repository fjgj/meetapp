package org.hablapps.meetup.funz.mysql

import scala.concurrent.{Future, ExecutionContext}

import org.hablapps.meetup.common.logic.Domain._
import org.hablapps.meetup.common.mysql.Domain._
import org.hablapps.meetup.funz.logic, logic._

import scalaz.{Store => ScalazStore, _}, Scalaz._

object Interpreter{
  import dbConfig._, driver.api._

  type WrongOrRight[x] = \/[StoreError, x]
  type AsyncResponse[x] = EitherT[Future,StoreError,x]

  case class RunInstruction(implicit ec: ExecutionContext) 
    extends (StoreInstruction ~> AsyncResponse) {
  
    def apply[T](instruction: StoreInstruction[T]): AsyncResponse[T] = instruction match {
      
      case GetGroup(gid: Int) => 
        EitherT(
          db.run(group_table.byID(Some(gid)).result.headOption)
            .map(_.fold[WrongOrRight[Group]](-\/(NonExistentEntity(gid)))(\/.right))
        )

      case GetUser(uid: Int) =>
        EitherT(
          db.run(user_table.byID(Some(uid)).result.headOption)
            .map(_.fold[WrongOrRight[User]](-\/(NonExistentEntity(uid)))(\/.right))
        )
    
      case PutJoin(join: JoinRequest) => 
          EitherT(
            db.run((join_table returning join_table.map(_.jid)
                into ((req,id) => req.copy(jid = id))) += join)
              .map(\/.right)
          )

      case PutMember(member: Member) =>
        val maybeId = member_table returning member_table.map(_.mid) += member
        EitherT(
            db.run((member_table returning member_table.map(_.mid)
                into ((mem,id) => mem.copy(mid = id))) += member)
              .map(\/.right)
        )

    }
  }

  def run[T](store: Store[T]): Future[\/[StoreError, T]] = {
    implicit val ec = ExecutionContext.Implicits.global
    store.foldMap(RunInstruction.apply).run
  }

}
