import Access "../backend/Access";
import Array "mo:base/Array";
import Base "../backend/Base";
import Buffer "mo:base/Buffer";
import Debug "mo:base/Debug";
import Demo "../backend/Demo";
import HashMap "mo:base/HashMap";
import Int "mo:base/Int";
import Iter "mo:base/Iter";
import List "mo:base/List";
import Nat "mo:base/Nat";
import Option "mo:base/Option";
import P "mo:base/Prelude";
import Param "../backend/Param";
import Prelude "mo:base/Prelude";
import Principal "mo:base/Principal";
import Rel "../backend/Rel";
import RelObj "../backend/RelObj";
import State "../backend/State";
import Text "mo:base/Text";
import Time "mo:base/Time";
import TrieMap "mo:base/TrieMap";
import Types "../backend/Types";

shared ({caller = initPrincipal}) actor class CanCan () /* : Types.Service */ {

  public type ProfileInfo = Types.ProfileInfo;
  public type ProfileInfoPlus = Types.ProfileInfoPlus;
  public type ProfilePic = Types.ProfilePic;
  public type Timestamp = Types.Timestamp;
  public type UserId = Types.UserId;
  public type VideoId = Types.VideoId;
  public type ChunkId = Types.ChunkId;
  public type ChunkData = Types.ChunkData;
  public type VideoInfo = Types.VideoInfo;
  public type VideoInit = Types.VideoInit;
  public type VideoPic = Types.VideoPic;
  public type VideoResult = Types.VideoResult;
  public type VideoResults = Types.VideoResults;

  var state = State.empty({ admin = initPrincipal });

  /*public query*/ func getState() : async State.StateShared {
    State.share(state)
  };

  /*public*/ func setState(st : State.StateShared) : async () {
    state := State.fromShared(st);
  };

  public query func checkUsernameAvailable(userName_ : Text): async Bool {
    switch (state.profiles.get(userName_)) {
      case (?_) { /* error -- ID already taken. */ false };
      case null { /* ok, not taken yet. */ true };
    }
  };

  /// null means that the principal is unrecognized,
  /// otherwise, returns a non-empty array of usernames.
  public shared(msg) func getUserNameByPrincipal(p:Principal) : async ?[Text] {
    if ( msg.caller == p ) {
      ?state.access.userPrincipal.get1(p)
    } else {
      // access control check fails; do not reveal username of p.
      null
    }
  };

  // responsible for adding metadata from the user to the state.
  // a null principal means that the username has no valid callers (yet), and the admin
  // must relate one or more principals to it.
  func createProfile_(userName_ : Text, p: ?Principal, pic_ : ?ProfilePic) : ?ProfileInfoPlus {
    switch (state.profiles.get(userName_)) {
      case (?_) { /* error -- ID already taken. */ null };
      case null { /* ok, not taken yet. */
        let now = timeNow_();
        state.profiles.put(userName_, {
            userName = userName_ ;
            createdAt = now ;
        });
        // rewards init invariant: rewards is initialized to zero (is non-null).
        state.rewards.put(userName_, 0);
        state.eventLog.add({
          time = now;
          kind = #createProfile({userName=userName_; pic=pic_})
        });
        state.access.userRole.put(userName_, #user);
        switch p {
          case null { }; // no related principals, yet.
          case (?p) { state.access.userPrincipal.put(userName_, p); }
        };
        // return the full profile info
        getProfilePlus_(userName_)
      };
    }
  };

  func accessCheck(caller : Principal, action : Types.UserAction, target : Types.ActionTarget) : ?() {
    state.access.check(timeNow_(), caller, action, target)
  };

  public shared(msg) func createProfile(userName : Text, pic : ?ProfilePic) : async ?ProfileInfoPlus {
    do ? {
      accessCheck(msg.caller, #create, #user userName)!;
      createProfile_(userName, ?msg.caller, pic)!
    }
  };

  var timeMode : {#ic ; #script} =
    switch (Param.timeMode) {
     case (#ic) #ic;
     case (#script _) #script
    };

  var scriptTime : Int = 0;

  func timeNow_() : Int {
    switch timeMode {
      case (#ic) { Time.now() };
      case (#script) { scriptTime };
    }
  };

  public shared(msg) func scriptTimeTick() : async ?() {
    do ? {
      accessCheck(msg.caller, #admin, #all)!;
      assert (timeMode == #script);
      scriptTime := scriptTime + 1;
    }
  };

  func reset_( mode : { #ic ; #script : Int } ) {
    setTimeMode_(mode);
    state := State.empty({ admin = state.access.admin });
  };

  public shared(msg) func reset( mode : { #ic ; #script : Int } ) : async ?() {
    do ? {
      accessCheck(msg.caller, #admin, #all)!;
      reset_(mode)
    }
  };

  func setTimeMode_( mode : { #ic ; #script : Int } ) {
    switch mode {
      case (#ic) { timeMode := #ic };
      case (#script st) { timeMode := #script ; scriptTime := st };
    }
  };

  public shared(msg) func setTimeMode( mode : { #ic ; #script : Int } ) : async ?() {
    do ? {
      accessCheck(msg.caller, #admin, #all)!;
      setTimeMode_(mode)
    }
  };

  func getProfileInfo_(userId : UserId) : ?ProfileInfo {
    do ? {
      let profile = state.profiles.get(userId)!;
      let following_ = state.follows.get0(userId);
      let followers_ = state.follows.get1(userId);
      let likes_ = state.likes.get0(userId);
      let superLikes_ = state.superLikes.get0(userId);
      let uploaded_ = state.uploaded.get0(userId);
      let rewards_ = state.rewards.get(userId)!;
      let abuseFlags_ = state.abuseFlagVideos.get1Size(userId) ;
      {
        userName = profile.userName ;
        followers = filterOutAbuseUsers(followers_)! ;
        following = filterOutAbuseUsers(following_)! ;
        likedVideos = filterOutAbuseVideos(likes_)! ;
        superLikedVideos = filterOutAbuseVideos(superLikes_)! ;
        uploadedVideos = filterOutAbuseVideos(uploaded_)! ;
        hasPic = false ;
        rewards = rewards_;
        abuseFlags = abuseFlags_; // count other users' abuse flags on this user's profile.
      }
    }
  };

  public query(msg) func getProfileInfo(userId : UserId) : async ?ProfileInfo {
    do ? {
      accessCheck(msg.caller, #view, #user userId)!;
      getProfileInfo_(userId)!
    }
  };

  // this is a deeper serialization of the user's profile so that we can do less work on the front-end.
  //  I opted not to replace "getProfileInfo" as this is possibly bad code I (Andrew) have written, and don't want to
  // fully commit :D -AW
  public query(msg) func getProfilePlus(userId: UserId): async ?ProfileInfoPlus {
    do ? {
      accessCheck(msg.caller, #view, #user userId)!;
      getProfilePlus_(userId)!
    }
  };

  func filterOutAbuseVideos(videos: [VideoId]) : ?[VideoId] {
    do ? {
      let nonAbuse = Buffer.Buffer<VideoId>(0);
      for (v in videos.vals()) {
        let flags = state.abuseFlagVideos.get1Size(v);
        if (flags < Param.contentModerationThreshold) {
          nonAbuse.add(v)
        }
      };
      nonAbuse.toArray()
    }
  };

  func filterOutAbuseUsers(users: [UserId]) : ?[UserId] {
    do ? {
      let nonAbuse = Buffer.Buffer<UserId>(0);
      for (u in users.vals()) {
        let flags = state.abuseFlagUsers.get1Size(u);
        if (flags < Param.contentModerationThreshold) {
          nonAbuse.add(u)
        }
      };
      nonAbuse.toArray()
    }
  };

  func getNonAbuseVideos(videos: [VideoId]) : ?[VideoInfo] {
    do ? {
      let nonAbuse = Buffer.Buffer<VideoInfo>(0);
      for (v in videos.vals()) {
        let flags = state.abuseFlagVideos.get1Size(v);
        if (flags < Param.contentModerationThreshold) {
          nonAbuse.add(getVideoInfo_(v)!)
        }
      };
      nonAbuse.toArray()
    }
  };

  func getNonAbuseProfiles(users: [UserId]) : ?[ProfileInfo] {
    do ? {
      let nonAbuse = Buffer.Buffer<ProfileInfo>(0);
      for (u in users.vals()) {
        let flags = state.abuseFlagUsers.get1Size(u);
        if (flags < Param.contentModerationThreshold) {
          nonAbuse.add(getProfileInfo_(u)!)
        }
      };
      nonAbuse.toArray()
    }
  };

  func getProfilePlus_(userId: UserId): ?ProfileInfoPlus {
    //  TODO: add auth
    do ? {
      let profile = state.profiles.get(userId)!;
      {
        userName = profile.userName;
        following = getNonAbuseProfiles(state.follows.get0(userId))!;
        followers = getNonAbuseProfiles(state.follows.get1(userId))!;
        likedVideos = getNonAbuseVideos(state.likes.get0(userId))!;
        uploadedVideos = getNonAbuseVideos(state.uploaded.get0(userId))!;
        hasPic = false;
        rewards = state.rewards.get(userId)!;
        abuseFlags = state.abuseFlagUsers.get1Size(userId) ; // other users' flags on this one.
      }
    }
 };

  public query(msg) func getProfiles() : async ?[ProfileInfo] {
    do ? {
      accessCheck(msg.caller, #admin, #all)!;
      let b = Buffer.Buffer<ProfileInfo>(0);
      for ((p, _) in state.profiles.entries()) {
        b.add(getProfileInfo_(p)!)
      };
      b.toArray()
    }
  };

  public query(msg) func getVideos() : async ?[VideoInfo] {
    do ? {
      accessCheck(msg.caller, #admin, #all)!;
      let b = Buffer.Buffer<VideoInfo>(0);
      for ((v, _) in state.videos.entries()) {
        b.add(getVideoInfo_(v)!)
      };
      b.toArray()
    }
  };

  public query(msg) func getProfilePic(userId : UserId) : async ?ProfilePic {
    do ? {
      accessCheck(msg.caller, #view, #user userId)!;
      state.profilePics.get(userId)!
    }
  };

  public shared(msg) func putRewards(
    receiver : UserId,
    amount : Nat
  ) : async ?() {
    do ? {
      accessCheck(msg.caller, #admin, #user receiver)!;
      let bal = state.rewards.get(receiver)!;
      state.rewards.put(receiver, bal + amount);
    }
  };

  public shared(msg) func putRewardTransfer(
    sender : UserId,
    receiver : UserId,
    amount : Nat
  ) : async ?() {
    do ? {
      accessCheck(msg.caller, #update, #user sender)!;
      putRewardTransfer_(sender, receiver, amount)!
    }
  };

  func putRewardTransfer_(sender : UserId,
                          receiver : UserId, amount : Nat) : ?() {
    do ? {
      let balSrc = state.rewards.get(sender)!;
      let balTgt = state.rewards.get(receiver)!;
      if (balSrc >= amount) {
        state.rewards.put(sender, balSrc - amount);
        state.rewards.put(receiver, balTgt + amount);

        state.messages.put(receiver,
           { time = timeNow_();
             event = #transferReward {
               rewards = amount;
             }
           });
        state.eventLog.add({
          time = timeNow_();
          kind = #rewardPointTransfer({sender = sender; receiver = receiver; amount = amount})
        });

      } else { return null }
    }
  };

  public shared(msg) func putProfilePic(userId : UserId, pic : ?ProfilePic) : async ?() {
    do ? {
      accessCheck(msg.caller, #update, #user userId)!;
      switch pic {
      case (?pic) { state.profilePics.put(userId, pic) };
      case null { ignore state.profilePics.remove(userId) };
      }
    }
  };

  func getVideoResult(i : VideoId) : ?VideoResult {
    do ? {
      (getVideoInfo_(i)!, state.videoPics.get(i))
    }
  };

  func getUserUploaded(userId : UserId, limit : ?Nat) : ?VideoResults {
    do ? {
      let buf = Buffer.Buffer<VideoResult>(0);
      for (vid in state.uploaded.get0(userId).vals()) {
        buf.add((getVideoResult vid)!)
      };
      buf.toArray()
    }
  };

  func getFeedVideos_(userId : UserId, limit : ?Nat) : ?VideoResults {
    do ? {
      let vids = HashMap.HashMap<Text, ()>(0, Text.equal, Text.hash);
      let _ = state.profiles.get(userId)!; // assert userId exists
      let buf = Buffer.Buffer<VideoResult>(0);
      let followIds = state.follows.get0(userId);
      label loopFollows
      for (i in followIds.vals()) {
        switch limit { case null { }; case (?l) { if (buf.size() == l) { break loopFollows } } };
        let vs = getUserUploaded(i, limit)!;
        for ((vi, vp) in vs.vals()) {
          if (vids.get(vi.videoId) == null) {
            vids.put(vi.videoId, ());
            buf.add((vi, vp));
          }
        }
      };
      label loopAll
      for ((vid, v) in state.videos.entries()) {
        switch limit { case null { }; case (?l) { if (buf.size() == l) { break loopAll } } };
        if (vids.get(vid) == null) {
            vids.put(vid, ());
            let vPic = state.videoPics.get(vid);
            let vi = getVideoInfo_(vid)!;
            buf.add((vi, vPic));
        }
      };
      buf.toArray()
    }
  };

  public query(msg) func getFeedVideos(userId : UserId, limit : ?Nat) : async ?VideoResults {
    do ? {
      accessCheck(msg.caller, #view, #user userId)!;
      getFeedVideos_(userId, limit)!
    }
  };

  public query(msg) func getProfileVideos(i : UserId, limit : ?Nat) : async ?VideoResults {
    do ? {
      accessCheck(msg.caller, #view, #user i)!;
      let buf = Buffer.Buffer<VideoResult>(0);
      let vs = getUserUploaded(i, limit)!;
      for (v in vs.vals()) {
        buf.add(v)
      };
      buf.toArray()
    }
  };

  public query(msg) func getSearchVideos(userId : UserId, terms : [Text], limit : ?Nat) : async ?VideoResults {
    do ? {
      accessCheck(msg.caller, #view, #user userId)!;
      getFeedVideos_(userId, limit)!;
    }
  };

  // check if adding the source-target pair "now" in the log is valid.
  // needed here (backend logic) and by front-end logic, when rendering enabled/disabled button status for superLike controls.
  func getSuperLikeValidNow_(source : UserId, target : VideoId) : Bool {
    let notRecent = timeNow_() - Param.recentPastDuration;
    let superLiked = HashMap.HashMap<Text, Bool>(0, Text.equal, Text.hash);
    superLiked.put(target, true);
    var count = 1;

    label hugeLog
    for (ev in state.eventLog.revVals()) {
      if(ev.time <= notRecent){ break hugeLog };
      switch (ev.kind) {
        case (#superLikeVideo(sl)) {
               if(sl.source == source and superLiked.get(sl.target) == null){
                   superLiked.put(sl.target, sl.superLikes);
                   if (sl.superLikes) {
                       count += 1;
                   };
               }
             };
        case _ { };
      };
    };

    count <= Param.maxRecentSuperLikes;
  };

  func videoIsViral(videoId : VideoId) : Bool {
    Option.isSome(do ? {
          let v = state.videos.get(videoId)!;
          v.viralAt!
    })
  };

  // check if we need to emit viral video signal to CanCan logic.
  func checkEmitVideoViral_(video : VideoId) {
    let vinfo = Option.unwrap(state.videos.get(video));
    if (Option.isSome(vinfo.viralAt)) {
        return;
    };

    let now = timeNow_();
    let notRecent = now - Param.recentPastDuration;
    let superLiked = HashMap.HashMap<Text, Bool>(0, Text.equal, Text.hash);
    let superLikers = Buffer.Buffer<State.Event.ViralVideoSuperLiker>(0);

    label hugeLog
    for (ev in state.eventLog.revVals()) {
      if(ev.time <= notRecent){ break hugeLog };
      switch (ev.kind) {
        case (#superLikeVideo(sl)) {
               if(sl.target == video and superLiked.get(sl.source) == null) {
                   superLiked.put(sl.source, sl.superLikes);
                   // The last sl.superLikes decide whether the user eventually liked the video or not
                   if (sl.superLikes) {
                       superLikers.add({ user = sl.source ; time = ev.time });
                   };
               }
             };
        case _ { };
      };
    };

    if(superLikers.size() >= Param.superLikeViralThreshold) {
        state.videos.put(video,
                         {
                             userId = vinfo.userId ;
                             uploadedAt = vinfo.uploadedAt ;
                             viewCount = vinfo.viewCount ;
                             createdAt = vinfo.createdAt ;
                             viralAt = ?now;
                             caption = vinfo.caption ;
                             tags = vinfo.tags ;
                             name = vinfo.name ;
                             chunkCount = vinfo.chunkCount ;
                         });
        /*
        state.eventLog.add({time=now;
                            kind=#emitSignal(
                              #viralVideo{
                                  video=video;
                                  uploader=vinfo.userId;
                                  superLikers=superLikers.toArray()}
                            )});*/
        let score = Option.get(state.rewards.get(vinfo.userId), 0);
        state.rewards.put(vinfo.userId, score + Param.rewardsForUploader);
        state.messages.put(vinfo.userId,
                           { time = now;
                             event = #uploadReward {
                                 rewards = Param.rewardsForUploader;
                                 videoId = video;
                             }
                           });
        for (id in superLikers.vals()) {
            let score = Option.get(state.rewards.get(id.user), 0);
            state.rewards.put(id.user, score + Param.rewardsForSuperliker);
            state.messages.put(id.user,
                               { time = now;
                                 event = #superlikerReward {
                                     rewards = Param.rewardsForSuperliker;
                                     videoId = video;
                                 }
                               });
        };
    }
  };

  public query(msg) func getMessages(user: UserId) : async ?[Types.Message] {
    do ? {
      accessCheck(msg.caller, #view, #user user)!;
      state.messages.get0(user)
    }
  };

  public query(msg) func isDropDay() : async ?Bool {
    do ? {
      accessCheck(msg.caller, #view, #pubView)!;
      let now = timeNow_();
      now % (Param.dropDayDuration + Param.dropDayNextDuration) < Param.dropDayDuration
    }
  };

  public query(msg) func getSuperLikeValidNow(source : UserId, target : VideoId) : async ?Bool {
    do ? {
      accessCheck(msg.caller, #view, #user target)!;
      getSuperLikeValidNow_(source, target)
    }
  };

  public query(msg) func getIsSuperLiker(source : UserId, target : VideoId) : async ?Bool {
    do ? {
      accessCheck(msg.caller, #view, #user target)!;
      state.superLikes.isMember(source, target)
    }
  };

  func putSuperLike_(userId : UserId, videoId : VideoId, superLikes_ : Bool) : ?() {
    do ? {
      let _ = state.videos.get(videoId)!; // assert that the videoId is valid
      if superLikes_ {
        if (getSuperLikeValidNow_(userId, videoId)) {
          state.superLikes.put(userId, videoId);
          state.eventLog.add({ time = timeNow_();
                               kind = #superLikeVideo({ source = userId ;
                                                        target = videoId ;
                                                        superLikes = true }
                               )});
          checkEmitVideoViral_(videoId);
        } else {
          state.eventLog.add({ time = timeNow_();
                               kind = #superLikeVideoFail({ source = userId ;
                                                            target = videoId }
                               )});
          return null // fail
        }
      } else {
        state.superLikes.delete(userId, videoId);
        state.eventLog.add({ time = timeNow_();
                             kind = #superLikeVideo({ source = userId ;
                                                      target = videoId ;
                                                      superLikes = false }
                             )});
      }
    }
  };

  public shared(msg) func putSuperLike
    (userId : UserId, videoId : VideoId, willSuperLike : Bool) : async ?() {
    do ? {
      accessCheck(msg.caller, #update, #user userId)!;
      putSuperLike_(userId, videoId, willSuperLike)!
    }
  };

  public shared(msg) func putProfileVideoLike
    (userId : UserId, videoId : VideoId, willLike_ : Bool) : async ?() {
    do ? {
      accessCheck(msg.caller, #update, #user userId)!;
      if willLike_ {
        state.likes.put(userId, videoId);
      } else {
        state.likes.delete(userId, videoId)
      };
      state.eventLog.add({ time = timeNow_();
                           kind = #likeVideo({ source = userId ;
                                               target = videoId ;
                                               likes = willLike_ }
                           )});
    }
  };

  func putProfileFollow_
    (userId : UserId, followedBy : UserId, follows : Bool) : ?() {
    if (userId == followedBy) { return null };
    if follows {
      state.follows.put(userId, followedBy)
    } else {
      state.follows.delete(userId, followedBy)
    };
    ?()
  };

  public shared(msg) func putProfileFollow
    (userId : UserId, toFollow : UserId, follows : Bool) : async ?() {
    do ? {
      accessCheck(msg.caller, #update, #user userId)!;
      putProfileFollow_(userId, toFollow, follows)!
    }
  };

  // internal function for adding metadata
  func createVideo_(i : VideoInit) : ?VideoId {
    let now = timeNow_();
    let videoId = i.userId # "-" # i.name # "-" # (Int.toText(now));
    switch (state.videos.get(videoId)) {
    case (?_) { /* error -- ID already taken. */ null };
    case null { /* ok, not taken yet. */
           state.videos.put(videoId,
                            {
                              videoId = videoId;
                              userId = i.userId ;
                              name = i.name ;
                              createdAt = i.createdAt ;
                              uploadedAt = now ;
                              viralAt = null ;
                              caption =  i.caption ;
                              chunkCount = i.chunkCount ;
                              tags = i.tags ;
                              viewCount = 0 ;
                            });
           state.uploaded.put(i.userId, videoId);
           state.eventLog.add({ time = now;
                                kind = #createVideo({info = i}) });
           ?videoId
         };
    }
  };

  public shared(msg) func createVideo(i : VideoInit) : async ?VideoId {
    do ? {
      accessCheck(msg.caller, #update, #user(i.userId))!;
      createVideo_(i)!
    }
  };

  func getVideoInfo_ (videoId : VideoId) : ?VideoInfo {
    do ? {
      let v = state.videos.get(videoId)!;
      {
        videoId = videoId;
        pic = state.videoPics.get(videoId);
        userId = v.userId ;
        createdAt = v.createdAt ;
        uploadedAt = v.uploadedAt ;
        viralAt = v.viralAt ;
        caption = v.caption ;
        tags = v.tags ;
        likes = state.likes.get1(videoId);
        superLikes = state.superLikes.get1(videoId);
        viewCount = v.viewCount ;
        name = v.name ;
        chunkCount = v.chunkCount ;
        // This implementation makes public all users who flagged every video,
        // but if that information should be kept private, get video info
        // could return just whether the calling user flagged it.
        abuseFlagUsers = state.abuseFlagVideos.get1(videoId) ;
      }
    }
  };

  public query(msg) func getVideoInfo (videoId : VideoId) : async ?VideoInfo {
    do ? {
      accessCheck(msg.caller, #view, #video videoId)!;
      getVideoInfo_(videoId)!
    }
  };

  public query(msg) func getVideoPic(videoId : VideoId) : async ?VideoPic {
    do ? {
      accessCheck(msg.caller, #view, #video videoId)!;
      state.videoPics.get(videoId)!
    }
  };

  public shared(msg) func putVideoInfo(videoId : VideoId, videoInit : VideoInit) : async ?() {
    do ? {
      accessCheck(msg.caller, #update, #video videoId)!;
      let i = videoInit ;
      let v = state.videos.get(videoId)!;
      state.videos.put(videoId,
                       {
                         // some fields are "immutable", regardless of caller data:
                         userId = v.userId ;
                         uploadedAt = v.uploadedAt ;
                         viewCount = v.viewCount ;
                         videoId = videoId ;
                         // -- above uses old data ; below is from caller --
                         createdAt = i.createdAt ;
                         viralAt = null;
                         caption = i.caption ;
                         tags = i.tags ;
                         name = i.name ;
                         chunkCount = i.chunkCount ;
                       })
    }
  };

  /// An abuse flag for a video occurs when a reporting user
  /// sets or clears the abuse toggle in their UI for the video.
  public shared (msg) func putAbuseFlagVideo
    (reporter : UserId, target : VideoId, abuseFlag : Bool) : async ?() {
    do ? {
      accessCheck(msg.caller, #update, #user reporter)!;
      state.eventLog.add({ time = timeNow_();
                           kind = #abuseFlag({ reporter = reporter ;
                                               target = #video(target);
                                               flag = abuseFlag })});
      if abuseFlag {
        state.abuseFlagVideos.put(reporter, target)
      } else {
        state.abuseFlagVideos.delete(reporter, target)
      };
    }
  };

  /// An abuse flag for a user occurs when a reporting user
  /// sets or clears the abuse toggle in their UI for the target user.
  public shared(msg) func putAbuseFlagUser
    (reporter : UserId, target : UserId, abuseFlag : Bool) : async ?() {
    do ? {
      accessCheck(msg.caller, #update, #user reporter)!;
      state.eventLog.add({ time = timeNow_();
                           kind = #abuseFlag({ reporter = reporter ;
                                               target = #user(target);
                                               flag = abuseFlag })});
      if abuseFlag {
        state.abuseFlagUsers.put(reporter, target)
      } else {
        state.abuseFlagUsers.delete(reporter, target)
      }
    }
  };

  public shared(msg) func putVideoPic(videoId : VideoId, pic : ?VideoPic) : async ?() {
    do ? {
      accessCheck(msg.caller, #update, #video videoId)!;
      switch pic {
      case (?pic) { state.videoPics.put(videoId, pic) };
      case null {
             switch (state.videoPics.remove(videoId)) {
             case null { /* not found */ return null };
             case _ { /* found and removed. */ };
             }
           };
      }
    }
  };

  func chunkId(videoId : VideoId, chunkNum : Nat) : ChunkId {
    videoId # (Nat.toText(chunkNum))
  };

  public shared(msg) func putVideoChunk
    (videoId : VideoId, chunkNum : Nat, chunkData : [Nat8]) : async ?()
  {
    do ? {
      accessCheck(msg.caller, #update, #video videoId)!;
      state.chunks.put(chunkId(videoId, chunkNum), chunkData);
    }
  };

  public query(msg) func getVideoChunk(videoId : VideoId, chunkNum : Nat) : async ?[Nat8] {
    do ? {
      accessCheck(msg.caller, #view, #video videoId)!;
      state.chunks.get(chunkId(videoId, chunkNum))!
    }
  };

  func createTestData_(users : [UserId], videos : [(UserId, VideoId)]) : ?() {
    do ? {
      for (u in users.vals()) {
        let _ = createProfile_(u, null, null)!;
      };
      for ((u, v) in videos.vals()) {
        let _ = createVideo_(
          {userId = u ;
           name = v ;
           createdAt = timeNow_() ;
           chunkCount = 0;
           caption = "";
           tags = [ ];})!;
      };
    }
  };

  public shared(msg) func createTestData(users : [UserId], videos : [(UserId, VideoId)]) : async ?() {
    do ? {
      accessCheck(msg.caller, #admin, #all)!;
      createTestData_(users, videos)!
    }
  };

  public shared(msg) func putTestFollows(follows : [(UserId, UserId)]) : async ?() {
    do ? {
      accessCheck(msg.caller, #admin, #all)!;
      for ((u, v) in follows.vals()) {
        let _ = putProfileFollow_(u, v, true)!;
      }
    }
  };

  public query(msg) func getEventLog() : async ?[State.Event.Event] {
    do ? {
      accessCheck(msg.caller, #admin, #all)!;
      Iter.toArray(state.eventLog.vals())
    }
  };

  public query(msg) func getAccessLog() : async ?[Access.Log.Event.Event] {
    do ? {
      accessCheck(msg.caller, #admin, #all)!;
      Iter.toArray(state.access.log.vals())
    }
  };

  func doDemo_(script : [Demo.Command]) : Demo.Trace {
    let trace = Buffer.Buffer<Demo.TraceCommand>(0);
    let r = do ? {
      for (cmd in script.vals()) {
        switch cmd {
        case (#reset(tm)) {
               reset_(tm); // discards trace
               trace.add({ command = cmd ;
                           result = #ok });
             };
        case (#createTestData(td)) {
               let _ = createTestData_(td.users, td.videos)!;
               trace.add({ command = cmd ;
                           result = #ok });
             };
        case (#putSuperLike(sl)) {
               let _ = putSuperLike_(sl.userId, sl.videoId, sl.superLikes)!;
               trace.add({ command = cmd ;
                           result = #ok });
             };
        case (#putProfileFollow(pf)) {
               let _ = putProfileFollow_(pf.userId, pf.toFollow, pf.follows)!;
               trace.add({ command = cmd ;
                           result = #ok });
             };
        case (#assertVideoFeed(vp)) {
               let vs : [VideoResult] = getFeedVideos_(vp.userId, vp.limit)!;
               let ids = Array.map<VideoResult, VideoId>
               (vs, func(vr : VideoResult) : VideoId { vr.0.videoId });
               let b = switch (vp.videosPred) {
                 case (#equals(expected)) {
                        Array.equal<VideoId>(ids, expected, Text.equal)
                      };
                 case (#containsAll(members)) {
                        Base.Array.containsAll<VideoId>(ids, members, Text.equal)
                      };
               };
               if b {
                 trace.add({ command = cmd ;
                             result = #ok });
               } else {
                 trace.add({ command = cmd ;
                             result = #err "video feed assertion failed"});
               }
             };
        case (#assertVideoVirality(avv)) {
               if (videoIsViral(avv.videoId) == avv.isViral) {
                 trace.add({ command = cmd ;
                             result = #ok });
               } else {
                 trace.add({ command = cmd ;
                             result = #err "viral assertion failed"});
               }
             };
        case (#putRewardTransfer(sra)) {
              switch (putRewardTransfer_(sra.sender, sra.receiver, sra.amount)) {
                case null {
                   trace.add({ command = cmd ;
                               result = #err "insufficient rewards"});
                };
                case (?()) {
                   trace.add({ command = cmd ;
                               result = #ok });
                }
              }
           };
        };
      };
    };
    // from option monad (above) to trace monad (below)
    let t = trace.toArray();
    switch r {
    case null { { status = #err ; trace = t } };
    case _ { { status = #ok ; trace = t } };
    }
  };

  public shared(msg) func doDemo(script : [Demo.Command]) : async ?Demo.Trace {
    do ? {
      accessCheck(msg.caller, #admin, #all)!;
      doDemo_(script)
    }
  };
}
