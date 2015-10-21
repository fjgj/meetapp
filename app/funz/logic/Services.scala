package org.hablapps.meetup.funz.logic

import org.hablapps.meetup.common.logic.Domain._

object Services{ 
  
  def join(request: JoinRequest): StoreProgram[JoinResponse] = for {
    _      <- StoreProgram.getUser(request.uid)
    group  <- StoreProgram.getGroup(request.gid)
    result <- Cond(
      test = group.must_approve,
      left = StoreProgram.putJoin(request),
      right = StoreProgram.putMember(Member(None, request.uid, request.gid))
    )
  } yield result 

}

