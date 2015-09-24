package org.hablapps.meetup.services

import org.hablapps.meetup.{domain, store}, store._, domain._

trait Services{ self: Store => 
  
  def join(request: JoinRequest): JoinResponse = {
    val JoinRequest(_, uid, gid) = request
    
    val _ = getUser(uid)
    val group = getGroup(gid)
    if (group.must_approve) 
      Left(putJoin(request))
    else
      Right(putMember(Member(None, uid, gid)))
  }

}

