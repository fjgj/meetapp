package org.hablapps.meetup.fun.logic

import org.hablapps.meetup.common.logic.Domain._

object Services{ 

  import Monad._

  def join[P[_]: StoreEff : MessagingEff: Monad](request: JoinRequest): P[JoinResponse] =
    for {
      _      <- StoreEff[P].getUser(request.uid)
      group  <- StoreEff[P].getGroup(request.gid)
      result <- MeetupProgram.Cond[P, JoinRequest, Member](
        test = group.must_approve,
        left = StoreEff[P].putJoin(request),
        right = for {
          member <- StoreEff[P].putMember(Member(None, request.uid, request.gid))
          _      <- MessagingEff[P].inform(request.uid, s"new member: $member")
        } yield member 
      )
    } yield result 

}

