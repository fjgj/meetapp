package org.hablapps.meetup.fun.logic

import org.hablapps.meetup.common.logic.Domain._

object Services{ 

  def joinRaw(request: JoinRequest): StoreProgram[JoinResponse] = 
    RunAndThen(Execute(GetUser(request.uid)), (user: User) => 
      RunAndThen(Execute(GetGroup(request.gid)), (group: Group) => 
        if (group.must_approve)
          RunAndThen(Execute(PutJoin(request)), (request: JoinRequest) => 
            Return(Left(request))
          )
        else {
          val new_member = Member(None, request.uid, request.gid)
          RunAndThen(Execute(PutMember(new_member)), (member: Member) => 
            Return(Right(member))
          )
        }
      )
    )

  import StoreProgram._
  def joinSmart(request: JoinRequest): StoreProgram[JoinResponse] = 
    RunAndThen(getUser(request.uid), (user: User) => 
      RunAndThen(getGroup(request.gid), (group: Group) => 
        if (group.must_approve)
          RunAndThen(putJoin(request), (request: JoinRequest) => 
            Return(Left(request))
          )
        else {
          val new_member = Member(None, request.uid, request.gid)
          RunAndThen(putMember(new_member), (member: Member) => 
            Return(Right(member))
          )
        }
      )
    )

  def joinMonadic(request: JoinRequest): StoreProgram[JoinResponse] = 
    getUser(request.uid) flatMap { (user: User) => 
      getGroup(request.gid) flatMap { (group: Group) => 
        if (group.must_approve)
          putJoin(request) flatMap { (request: JoinRequest) => 
            returns(Left(request))
          }
        else {
          val new_member = Member(None, request.uid, request.gid)
          putMember(new_member) flatMap { (member: Member) => 
            returns(Right(member))
          }
        }
      }
    }

  def join2(request: JoinRequest): StoreProgram[JoinResponse] = for {
    _      <- StoreProgram.getUser(request.uid)
    group  <- StoreProgram.getGroup(request.gid)
    result <- if (group.must_approve)
                StoreProgram.left(putJoin(request))
              else  
                StoreProgram.right(putMember(Member(None, request.uid, request.gid)))
  } yield result 

  def join(request: JoinRequest): StoreProgram[JoinResponse] = for {
    _      <- StoreProgram.getUser(request.uid)
    group  <- StoreProgram.getGroup(request.gid)
    result <- StoreProgram.Cond(
      test = group.must_approve,
      left = StoreProgram.putJoin(request),
      right = StoreProgram.putMember(Member(None, request.uid, request.gid))
    )
  } yield result 

}

