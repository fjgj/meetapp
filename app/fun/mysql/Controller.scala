package org.hablapps.meetup.fun.mysql

import scala.concurrent.{Future, ExecutionContext}
import ExecutionContext.Implicits.global

import play.api._
import play.api.mvc._
import play.api.libs.json._

import org.hablapps.meetup.common.logic.Domain._
import org.hablapps.meetup.fun.logic, logic._

object Members extends Controller{

  def add(gid: Int): Action[Int] =
    Action.async(parse.json[Int]) { 
      fromHTTP(gid)         andThen 
      logic.Services.join   andThen
      Interpreter.run       andThen
      toHTTP
    }

  def fromHTTP(gid: Int): Request[Int] => JoinRequest = 
    request => JoinRequest(None, request.body, gid)

  def toHTTP(response: Future[JoinResponse]): Future[Result] = 
    response.map{
      case Left(joinRequest) => 
        Accepted(s"Join request $joinRequest, left pending for futher processing")
      case Right(member) => 
        Created(Json.toJson(member)(Json.writes[Member]))
    }.recover{ 
      case error@NonExistentEntity(id) => 
        NotFound(s"${error.msg}")
      case error => 
        InternalServerError(error.toString)
    }

}