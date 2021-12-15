import Types "Types";

/// Demo script types
module {
  public type UserId = Types.UserId;
  public type VideoId = Types.VideoId;

  public type Command = {
    #reset : Types.TimeMode ;
    #putRewardTransfer : { sender : UserId ; receiver : UserId ; amount : Nat };
    #createTestData : { users : [UserId] ; videos : [(UserId, VideoId)] };
    #putSuperLike : { userId : UserId ; videoId : VideoId ; superLikes : Bool };
    #putProfileFollow : { userId : UserId ; toFollow : UserId ; follows : Bool }; 
    #assertVideoVirality : { videoId : VideoId ; isViral : Bool };
    #assertVideoFeed : { userId : UserId ; limit : ?Nat ; videosPred : VideosPred };
 };
  
  public type VideosPred = {
    #containsAll : [VideoId] ; // order independent check.
    #equals : [VideoId] ; // order dependent check.
  };

  public type Result = {
    #ok ;
    #err : Text
  };

  public type TraceCommand = {
    command : Command ;
    result : Result ;
  };

  public type Trace = {
    status : {#ok ; #err } ;
    trace : [TraceCommand]
  };

}
