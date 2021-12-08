import Text "mo:base/Text";
import Array "mo:base/Array";
import Principal "mo:base/Principal";

import Debug "mo:base/Debug";

import Types "Types";
import Role "Role";
import Rel "Rel";
import RelObj "RelObj";
import SeqObj "SeqObj";

module {

  /// Access control log stores all of the checks and their outcomes,
  // e.g., for debugging and auditing security.
  public module Log {
    public module Check {
      /// An access check consists of a caller, a username and a user action.
      public type Check = {
        caller : Principal;
        userAction : Types.UserAction;
        actionTarget : Types.ActionTarget
      };
    };
    public module Event {
      /// An access event is an access control check, its calling context, and its outcome.
      public type Event = {
        time : Int; // using mo:base/Time and Time.now() : Int
        check : Check.Check;
        isOk : Bool;
      };
      public func equal(x:Event, y:Event) : Bool { x == y };
    };
    public type Log = SeqObj.Seq<Event.Event>;
  };

  public class Access(
    init : {
    admin : Principal;
    uploaded : RelObj.RelObj<Types.UserId, Types.VideoId>
  } )
  {

    /// initial administrator (who grants admin role to others, potentially)
    public let admin : Principal = init.admin;

    /// access log.
    public var log : Log.Log =
      SeqObj.Seq<Log.Event.Event>(Log.Event.equal, null);

    /// # Definition of userPrincipal and userRole (general binary relations, not 1:1):
    ///
    /// - Each principal may be authorized with multiple usernames.
    /// - Those usernames may be shared among multiple principals.
    /// - Each username may have multiple roles,
    ///   especially as we introduce more moderation features, later.
    ///

    /// Relating usernames and roles.
    public var userRole : RelObj.RelObj<Types.UserId, Types.Role> =
      RelObj.RelObj<Types.UserId, Role.Role>
    ((Text.hash, Role.hash), (Text.equal, Role.equal));

    /// Relating usernames and (system-level) principal identifiers.
    public var userPrincipal : RelObj.RelObj<Types.UserId, Principal> =
      RelObj.RelObj<Types.UserId, Principal>
    ((Text.hash, Principal.hash), (Text.equal, Principal.equal));

    /// Get the maximal role for a user.
    public func userMaxRole(user : Types.UserId) : Types.Role {
      let roles = userRole.get0(user);
      switch (roles.size()) {
        case 0 { #guest };
        case 1 { roles[0] };
        case 2 { Role.max(roles[0], roles[1]) };
        case 3 { Role.max(roles[0], Role.max(roles[1], roles[2])) };
        case _ {
               // impossible, or broken invariants: only three possible roles.
               assert false;
               #guest
             };
      }
    };

    /// Get the maximal role for a caller,
    /// considering all possible user names associated with principal.
    public func callerMaxRole(p : Principal) : Types.Role {
      if (p == admin) { #admin } else {
        let usernames = userPrincipal.get1(p);
        let userRoles = Array.map<Types.UserId, Types.Role>(usernames, userMaxRole);
        Array.foldLeft(userRoles, #guest, Role.max)
      }
    };

    /// Perform a systematic (and logged) service-access check.
    ////
    /// `check(caller, userAction, userId)`
    /// checks that `userAction` is permitted by the caller as `userId`,
    /// returning `?()` if so, and `null` otherwise.
    ///
    /// This function is meant to be used as a protective guard,
    /// starting each service call, before any other CanCan service logic,
    /// (before it changes or accesses any state, to guard against unauthorized access).
    ///
    /// To audit the CanCan service for security, we need to check that this call is used
    /// appropriately in each call, and that its logic (below) is correct.
    ///
    /// In English, the logic is as follows:
    ///
    /// First, use the current state to resolve the caller Principal
    /// to all available roles, preferring the highest access according to the ordering:
    ///
    ///       (minimal access) #guest  <  #user  <  #admin (maximal access)
    ///
    /// The role #guest is for new Principals that are not recognized.
    ///
    /// Then, we apply these role-based rules:
    ///
    /// - #guest-role callers can create currently non-existent usernames,
    ///    and they may perform no other user actions (no updates and no views).
    ///
    /// - #user-role callers can update their own data,
    ///    and they may only view (not update) other users' data.
    ///
    /// - #admin-role callers can perform any action on any user (for simplicity, for now.
    ///   In a full production setting, we need audit trails, and different sub-roles for admins.)
    ///
    public func check(
      time_ : Int,
      caller_ : Principal,
      userAction_ : Types.UserAction,
      actionTarget_ : Types.ActionTarget,
    )
      : ?()
    {
      let result = switch (callerMaxRole(caller_)) {
        case (#admin) {
               // success; full power, and full responsibility.
               ?()
             };
        case (#guest) {
               // guests just create users.
               if(userAction_ == #create) { ?() }
               else { null }
             };
        case (#user) {
               switch userAction_ {
                 case (#view) { ?() };
                 case (#create) { ?() };
                 case (#admin) { null };
                 case (#update) {
                        switch actionTarget_ {
                          case (#pubView) { ?() };
                          case (#all) { null };
                          case (#user i) {
                                 if (userPrincipal.isMember(i, caller_)) {
                                   ?()
                                 } else { null }
                               };
                          case (#video v) {
                                 let users = init.uploaded.get1(v);
                                 switch (users.size()) {
                                 case 1 { if (userPrincipal.isMember(users[0], caller_)) {
                                            ?()
                                          } else { null } };
                                 // invariant: exactly only one video uploader per video.
                                 case _ { assert false; null };
                                 }
                               };
                        }
                      };
               }
             };

      };
      let accessEvent : Log.Event.Event =
        {
          time = time_;
          caller = caller_;
          isOk = result == ?();
          check = { caller = caller_;
                    userAction = userAction_;
                    actionTarget = actionTarget_ ; } };
      // print all access events for debugging
      Debug.print (debug_show accessEvent);
      // recall: this log will only be saved for updates, not queries; IC semantic rules.
      log.add(accessEvent);
      result
    };
  };
}
