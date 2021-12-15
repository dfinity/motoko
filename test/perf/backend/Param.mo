/// CanCan domain-logic parameters.
///
/// Parameters to the service's publicly visible behavior
module {
  /// Time mode.
  ///
  /// Controls how the actor records time and names unique IDs.
  ///
  /// For deployment (timeMode = #ic), the time is system time (nanoseconds since 1970-01-01).
  ///
  /// For scripts and CI-based testing, we want to predict and control time from a #script.
  public let timeMode : {#script; #ic} =
    #ic; // deterministic, small-number times in scripts.

  /// Super like limit.
  ///
  /// Users may not super like more than this many videos per "recent duration of time".
  public let maxRecentSuperLikes : Nat = 10;

  /// Super like viral threshold.
  ///
  /// When a video has this many super likes, the system "emits" a viralVideo signal.
  /// That signal triggers other actions within the domain logic of CanCan (TBD).
  public let superLikeViralThreshold : Nat = 5;

  /// Recent past duration of time.
  ///
  /// Users may not super like more than a limited number of videos per this duration of time.
  ///
  /// The units depends on the value of `timeMode`.
  /// Time units is nanoseconds (since 1970-01-01) when timeMMode is #ic.
  /// Time units is script-determined (often beginning at 0) when timeMode is #script.
  ///
  public let recentPastDuration : Int = 50_000_000_000;

  public let rewardsForUploader : Nat = 50;
  public let rewardsForSuperliker : Nat = 10;

  /// Duration for a drop day
  public let dropDayDuration : Nat = 1_000_000_000;
  /// Duration from the end of last drop day to the start the next drop
  public let dropDayNextDuration : Nat = 0_000_000_000;


  /// Content moderation
  ///
  /// Automated BE policy, based on a simple threshold.
  /// If more than this number of distinct users flag a video or user,
  /// the BE logic omits that user or video from all query results.
  /// They become "redacted" from the API results.
  ///
  /// Later, we can have more refined policies,
  /// perhaps based on more sophisticated distributed moderation practices.
  ///
  public let contentModerationThreshold : Nat = 2;
}
