import Hash "mo:base/Hash";
import Prelude "mo:base/Prelude";
import Text "mo:base/Text";
import Int "mo:base/Int";
import Trie "mo:base/Trie";
import TrieMap "mo:base/TrieMap";

// import non-base primitives
import Access "Access";
import Role "Role";
import Rel "Rel";
import RelObj "RelObj";
import SeqObj "SeqObj";

// types in separate file
import Types "./Types";

/// Internal CanCan canister state.
module {

  // Our representation of (binary) relations.
  public type RelShared<X, Y> = Rel.RelShared<X, Y>;
  public type Rel<X, Y> = RelObj.RelObj<X, Y>;

  // Our representation of finite mappings.
  public type MapShared<X, Y> = Trie.Trie<X, Y>;
  public type Map<X, Y> = TrieMap.TrieMap<X, Y>;

  public type ChunkId = Types.ChunkId;
  public type ChunkData = Types.ChunkData;

  public module Event {

    public type CreateProfile = {
      userName : Text;
      pic: ?Types.ProfilePic;
    };

    public type CreateVideo = {
      info : Types.VideoInit;
    };

    public type LikeVideo = {
      source : Types.UserId;
      target : Types.VideoId;
      likes : Bool; // false for an "unlike" event
    };

    public type SuperLikeVideo = {
      source : Types.UserId;
      target : Types.VideoId;
      superLikes : Bool; // false for an "un-Super-like" event
    };

    public type RewardPointTransfer = {
      sender : Types.UserId;
      receiver : Types.UserId;
      amount : Nat;
    };

    public type SuperLikeVideoFail = {
      source : Types.UserId;
      target : Types.VideoId;
    };

    /// A viral video signal.
    public type ViralVideo = {
      video : Types.VideoId;
      uploader : Types.UserId;
      superLikers : [ViralVideoSuperLiker];
    };

    /// A viral video signal query.
    public type ViralVideoQuery = {
      video : Types.VideoId;
    };

    /// A viral video's super liker is credited
    // based on time relative to other super likers.
    public type ViralVideoSuperLiker = {
      user : Types.UserId ;
      time : Int ;
    };

    /// A signal precipitates further autonomous events.
    /// (Like an "event continuation" within the DSL of events).
    public type Signal = {
      #viralVideo : ViralVideo;
    };

    public type SignalQuery = {
      #viralVideo : ViralVideoQuery
    };

    /// An abuse flag event occurs when a reporting user
    /// sets or clears the abuse toggle in their UI for a video or user.
    public type AbuseFlag = {
      reporter : Types.UserId;
      target : {
        #video : Types.VideoId;
        #user : Types.UserId;
      };
      flag : Bool;
    };

    public type EventKind = {
      #reset : Types.TimeMode;
      #createProfile : CreateProfile;
      #createVideo : CreateVideo;
      #likeVideo : LikeVideo;
      #superLikeVideo : SuperLikeVideo;
      #superLikeVideoFail : SuperLikeVideoFail;
      #rewardPointTransfer : RewardPointTransfer;
      #emitSignal : Signal;
      #abuseFlag : AbuseFlag;
    };

    public type Event = {
      time : Int; // using mo:base/Time and Time.now() : Int
      kind : EventKind;
    };

    public func equal(x:Event, y:Event) : Bool { x == y };
    public type Log = SeqObj.Seq<Event>;
  };

  /// State (internal CanCan use only).
  ///
  /// Not a shared type because of OO containers and HO functions.
  /// So, cannot send in messages or store in stable memory.
  ///
  public type State = {
    access : Access.Access;

    /// event log.
    eventLog : Event.Log;

    /// all profiles.
    profiles : Map<Types.UserId, Profile>;

    /// all profile pictures (aka thumbnails).
    profilePics : Map<Types.UserId, Types.ProfilePic>;

    rewards: Map<Types.UserId, Nat>;

    messages: Rel<Types.UserId, Types.Message>;

    /// all videos.
    videos : Map<Types.VideoId, Video>;

    /// all video pictures (aka thumbnails).
    videoPics : Map<Types.VideoId, Types.VideoPic>;

    /// follows relation: relates profiles and profiles.
    follows : Rel<Types.UserId, Types.UserId>;

    /// likes relation: relates profiles and videos.
    likes : Rel<Types.UserId, Types.VideoId>;

    /// super likes relation: relates profiles and videos.
    superLikes : Rel<Types.UserId, Types.VideoId>;

    /// uploaded relation: relates profiles and videos.
    uploaded : Rel<Types.UserId, Types.VideoId>;

    /// all chunks.
    chunks : Map<Types.ChunkId, ChunkData>;

    /// Users may place an abuse flag on videos and other users.
    abuseFlagUsers : Rel<Types.UserId, Types.UserId>;
    abuseFlagVideos : Rel<Types.UserId, Types.VideoId>;
  };

  // (shared) state.
  //
  // All fields have stable types.
  // This type can be stored in stable memory, for upgrades.
  //
  // All fields have shared types.
  // This type can be sent in messages.
  // (But messages may not benefit from tries; should instead use arrays).
  //
  public type StateShared = {
    /// all profiles.
    profiles : MapShared<Types.UserId, Profile>;

    /// all users. see andrew for disambiguation
    users : MapShared<Principal, Types.UserId>;

    /// all videos.
    videos : MapShared<Types.VideoId, Video>;

    rewards: MapShared<Types.UserId, Nat>;

    /// follows relation: relates profiles and profiles.
    follows : RelShared<Types.UserId, Types.UserId>;

    /// likes relation: relates profiles and videos.
    likes : RelShared<Types.UserId, Types.VideoId>;

    /// uploaded relation: relates profiles and videos.
    uploaded : RelShared<Types.UserId, Types.VideoId>;

    /// all chunks.
    chunks : MapShared<Types.ChunkId, ChunkData>;
  };

  /// User profile.
  public type Profile = {
    userName : Text ;
    createdAt : Types.Timestamp;
  };

  /// Video.
  public type Video = {
    userId : Types.UserId;
    createdAt : Types.Timestamp;
    uploadedAt : Types.Timestamp;
    viralAt: ?Types.Timestamp;
    caption: Text;
    tags: [Text];
    viewCount: Nat;
    name: Text;
    chunkCount: Nat;
  };

  public func empty (init : { admin : Principal }) : State {
    let equal = (Text.equal, Text.equal);
    let hash = (Text.hash, Text.hash);
    func messageEqual(a: Types.Message, b: Types.Message) : Bool = a == b;
    // not a very good hash, but we are not using the hash
    func messageHash(m: Types.Message) : Hash.Hash = Int.hash(m.time);
    let uploaded_ = RelObj.RelObj<Types.UserId, Types.VideoId>(hash, equal);
    let st : State = {
      access = Access.Access({ admin = init.admin ; uploaded = uploaded_ });
      profiles = TrieMap.TrieMap<Types.UserId, Profile>(Text.equal, Text.hash);
      rewards = TrieMap.TrieMap<Types.UserId, Nat>(Text.equal, Text.hash);
      messages = RelObj.RelObj((Text.hash, messageHash), (Text.equal, messageEqual));
      chunks = TrieMap.TrieMap<ChunkId, ChunkData>(Text.equal, Text.hash);
      profilePics = TrieMap.TrieMap<Types.UserId, Types.ProfilePic>(Text.equal, Text.hash);
      videos = TrieMap.TrieMap<Types.VideoId, Video>(Text.equal, Text.hash);
      videoPics = TrieMap.TrieMap<Types.VideoId, Types.VideoPic>(Text.equal, Text.hash);
      follows = RelObj.RelObj(hash, equal);
      likes = RelObj.RelObj(hash, equal);
      superLikes = RelObj.RelObj(hash, equal);
      uploaded = uploaded_;
      eventLog = SeqObj.Seq<Event.Event>(Event.equal, null);
      abuseFlagVideos = RelObj.RelObj(hash, equal);
      abuseFlagUsers = RelObj.RelObj(hash, equal);
    };
    st
  };

  public func share(state : State) : StateShared {
    Prelude.nyi() // to do -- for testing / upgrades sub-story
  };

  public func fromShared(share : StateShared) : State {
    Prelude.nyi() // to do -- for testing / upgrades sub-story
  };

}
