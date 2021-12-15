/// Public-facing types.
module {

public type Timestamp = Int; // See mo:base/Time and Time.now()

public type VideoId = Text; // chosen by createVideo
public type UserId = Text; // chosen by createUser
public type ChunkId = Text; // VideoId # (toText(ChunkNum))

public type ProfilePic = [Nat8]; // encoded as a PNG file
public type VideoPic = [Nat8]; // encoded as a PNG file
public type ChunkData = [Nat8]; // encoded as ???

/// Role for a caller into the service API.
/// Common case is #user.
public type Role = {
  // caller is a user
  #user;
  // caller is the admin
  #admin;
  // caller is not yet a user; just a guest
  #guest
};

/// Action is an API call classification for access control logic.
public type UserAction = {
  /// Create a new user name, associated with a principal and role #user.
  #create;
  /// Update an existing profile, or add to its videos, etc.
  #update;
  /// View an existing profile, or its videos, etc.
  #view;
  /// Admin action, e.g., getting a dump of logs, etc
  #admin
};

/// An ActionTarget identifies the target of a UserAction.
public type ActionTarget = {
  /// User's profile or videos are all potential targets of action.
  #user : UserId ;
  /// Exactly one video is the target of the action.
  #video : VideoId ;
  /// Everything is a potential target of the action.
  #all;
  /// Everything public is a potential target (of viewing only)
  #pubView
};

/// profile information provided by service to front end views -- Pic is separate query
public type ProfileInfo = {
 userName: Text;
 following: [UserId];
 followers: [UserId];
 uploadedVideos: [VideoId];
 likedVideos: [VideoId];
 hasPic: Bool;
 rewards: Nat;
 abuseFlags: Nat; // abuseFlags counts other users' flags on this profile, for possible blurring.
};

public type ProfileInfoPlus = {
  userName: Text;
 following: [ProfileInfo];
 followers: [ProfileInfo];
 uploadedVideos: [VideoInfo];
 likedVideos: [VideoInfo];
 hasPic: Bool;
 rewards: Nat;
 abuseFlags: Nat; // abuseFlags counts other users' flags on this profile, for possible blurring.
};

/// video information provided by front end to service, upon creation.
public type VideoInit = {
 userId : UserId;
 name: Text;
 createdAt : Timestamp;
 caption: Text;
 tags: [Text];
 chunkCount: Nat;
};

/// video information provided by service to front end views -- Pic is separate query
public type VideoInfo = {
 videoId : VideoId;
 userId : UserId;
 pic: ?VideoPic;
 createdAt : Timestamp;
 uploadedAt : Timestamp;
 viralAt: ?Timestamp;
 caption: Text;
 tags: [Text];
 likes: [UserId];
 superLikes: [UserId];
 viewCount: Nat;
 name: Text;
 chunkCount: Nat;
 abuseFlagUsers: [UserId]; // A lit of users that have flagged this video for abuse
};

public type VideoResult = (VideoInfo, ?VideoPic);
public type VideoResults = [VideoResult];

/// Notification messages
public type Message = {
    time: Timestamp;
    event: Event;
};
public type Event = {
    #uploadReward: { rewards: Nat; videoId: VideoId };
    #superlikerReward: { rewards: Nat; videoId: VideoId };
    #transferReward: { rewards: Nat };
};

/// For test scripts, the script controls how time advances, and when.
/// For real deployment, the service uses the IC system as the time source.
public type TimeMode = { #ic ; #script : Int };

/// CanCan canister's service type.
///
/// #### Conventions
///
/// - The service (not front end) generates unique ids for new profiles and videos.
/// - (On behalf of the user, the front end chooses the created profile's `userName`, not `userId`).
/// - Shared functions return `null` when given invalid IDs, or when they suffer other failures.
/// - The `Pic` param for putting Videos and Profiles is optional, and can be put separately from the rest of the info.
///   This de-coupled design is closer to how the front end used BigMap in its initial (current) design.
///
/// #### Naming conventions:
///
///  - three prefixes: `create`, `get` and `put`.
///  - `create`- prefix only for id-generating functions (only two).
///  - `get`- prefix for (query) calls that only ready data.
///  - `put`- prefix for (update) calls that overwrite data.
///
public type Service = actor {

  createProfile : (userName : Text, pic : ?ProfilePic) -> async ?UserId;
  getProfileInfo : query (userId : UserId) -> async ?ProfileInfo;
  getProfilePlus : query (userId : UserId) -> async ?ProfileInfoPlus;
  getProfilePic : query (userId : UserId) -> async ?ProfilePic;
  putProfilePic : (userId : UserId, pic : ?ProfilePic) -> async ?();

  getFeedVideos : /*query*/ (userId : UserId, limit : ?Nat) -> async ?VideoResults;
  getProfileVideos : /*query*/ (userId : UserId, limit : ?Nat) -> async ?VideoResults;
  getSearchVideos : query (userId : UserId, terms : [Text], limit : ?Nat) -> async ?VideoResults;

  putProfileVideoLike : (userId : UserId, videoId : VideoId, likes : Bool) -> async ?();
  putProfileFollow : (userId : UserId, toFollow : UserId, follow : Bool) -> async ?();

  createVideo : (videoInfo : VideoInfo) -> async ?VideoId;

  getVideoInfo : query (videoId : VideoId) -> async ?VideoInfo;
  getVideoPic  : query (videoId : VideoId) -> async ?VideoPic;

  putVideoInfo : (videoId : VideoId, videoInfo : VideoInfo) -> async ?();
  putVideoPic  : (videoId : VideoId, pic : ?VideoPic) -> async ?();

  putVideoChunk : (videoId : VideoId, chunkNum : Nat, chunkData : ChunkData) -> async ?();
  getVideoChunk : query (videoId : VideoId, chunkNum : Nat) -> async ?ChunkData;

};

}
