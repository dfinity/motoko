---
sidebar_position: 3
---

# Caller identification

[Principals](https://internetcomputer.org/docs/building-apps/canister-management/control) serve as unique identifiers for users, canisters, and other entities on ICP. Every user interacting with a canister will have a principal which acts as their [identity](https://internetcomputer.org/docs/building-apps/getting-started/identities) when making requests. Principals are privacy-preserving methods of authentication, meaning they can vary across different applications, preventing a user’s identity from being easily tracked across multiple dapps.

When a shared function is called, the caller’s [principal](https://internetcomputer.org/docs/building-apps/canister-management/control) is provided alongside the call to identify who initiated the request. This enables canisters to implement access control and restrict or allow certain actions based on the caller’s identity. A canister could enforce rules such as allowing only specific users to modify data, maintaining a list of authorized accounts, or preventing anonymous interactions.

The `shared` keyword is used to declare a shared function. A shared function can also declare an optional parameter of type `{caller : Principal}` to identify and verify request origins.

```motoko no-repl
shared(msg) func inc() : async () {
  let caller = msg.caller;
}
```

The caller’s principal is provided automatically and cannot be forged. This also applies to actor class constructors, where the installer's principal can be stored for access control.

```motoko no-repl
shared(msg) persistent actor class Counter(init : Nat) {
  let owner = msg.caller;
}
```


## Access control models

Caller identification is used to authenticate and authorize data access and modification. Various access control models can be implemented, each showcasing a different approach to managing authorization dynamically. The selection shown here prioritizes models that are well-suited for canisters and decentralized applications while ensuring a diverse range of implementations.

| Model | Method | Example use case |
|------------|----------------|---------------------------|
| Discretionary access control (DAC) | The owner manually grants and revokes access to specific users. | **User-managed whitelist**: Only users added by the owner can access a function. |
| Role-based access control (RBAC) | Users are assigned roles (`admin`, `member`), and permissions are granted based on role. | **Admins manage members**: Members have restricted access to specific functions. |
| Mandatory access control (MAC) | Access is enforced by system-wide rules that users cannot modify. | **Immutable permissions**: Read-only vs. write-only users set at account creation. |
| Access control list (ACL) | Different functions have separate whitelists, controlling access per action. | One list for `update`, another for `delete`, managed independently. |
| Attribute-based access control (ABAC) | Users must meet certain conditions to gain access. | **Exclusive model**: Users must hold at least **X tokens** to access the canister. |
| Policy-based access control (PBAC) | Policies dynamically restrict access based on conditions. | **Time-based access** : Users can only call functions during specific hours. |

<!---I will probably eliminate the use of any upgrade hooks in the second review as it's not recommended for now they are present--->

### Discretionary access control

Discretionary access control (DAC) allows the [controller](https://internetcomputer.org/docs/building-apps/canister-management/control) of a canister to manage an allow list, granting or revoking access dynamically. This approach is useful for whitelisting specific users or canisters for privileged actions while maintaining flexibility in managing permissions.

The following example implements a canister-controlled whitelist, where only authorized principals can execute restricted actions.

```motoko no-repl
import Principal "mo:base/Principal";
import OrderedMap "mo:base/OrderedMap";
import Error "mo:base/Error";

shared(msg) persistent actor class WhitelistAccess() {
  let controller = msg.caller; // The canister's deployer is the controller

  // Create an OrderedMap instance for Principal keys
  transient let principalMap = OrderedMap.Make<Principal>(Principal.compare);
  var whitelist : OrderedMap.Map<Principal, Bool> = principalMap.empty();

  // Add a principal to the whitelist
  public shared(msg) func addToWhitelist(user : Principal) : async () {
    if (msg.caller != controller) {
      throw Error.reject("Unauthorized: Only the controller can modify the whitelist.");
    };
    whitelist := principalMap.put(whitelist, user, true);
  };

  // Remove a principal from the whitelist
  public shared(msg) func removeFromWhitelist(user : Principal) : async () {
    if (msg.caller != controller) {
      throw Error.reject("Unauthorized: Only the controller can modify the whitelist.");
    };
    whitelist := principalMap.delete(whitelist, user);
  };

  // Check if a caller is in the whitelist
  public shared query(msg) func isWhitelisted() : async Bool {
    principalMap.get(whitelist, msg.caller) == ?true;
  };

  // Restricted function, accessible only to whitelisted users
  public composite query(msg) func restrictedAction() : async Text {
    if (not (await isWhitelisted())) {
      throw Error.reject("Access denied: Caller is not whitelisted.");
    };
    "Access granted to restricted action.";
  };
}
```

### Role based access control

Role-based access control (RBAC) assigns permissions based on roles rather than individual users. This allows for structured authorization, where users can be grouped into predefined roles with different access levels.

In this example, the canister assigns roles dynamically, allowing its controller to manage administrators and members, each with different permissions.

```motoko no-repl
import Principal "mo:base/Principal";
import OrderedMap "mo:base/OrderedMap";
import Error "mo:base/Error";

shared(msg) persistent actor class RoleBasedAccess() {
  let controller = msg.caller; // The canister's deployer is the initial controller

  // Define role types
  type Role = { #Admin; #Member };

  // OrderedMap instance for role storage
  let roleMap = OrderedMap.Make<Principal>(Principal.compare);
  stable var roles : OrderedMap.Map<Principal, Role> = roleMap.empty();

  // Assign a role to a principal
  public shared(msg) func assignRole(user : Principal, role : Role) : async () {
    if (msg.caller != controller) {
      throw Error.reject("Unauthorized: Only the controller can assign roles.");
    };
    roles := roleMap.put(roles, user, role);
  };

  // Remove a principal's role
  public shared(msg) func removeRole(user : Principal) : async () {
    if (msg.caller != controller) {
      throw Error.reject("Unauthorized: Only the controller can remove roles.");
    };
    roles := roleMap.delete(roles, user);
  };

  // Check a user's role
  public shared query(msg) func getRole() : async ?Role {
    roleMap.get(roles, msg.caller);
  };

  // Admin-only function
  public shared(msg) func adminAction() : async Text {
    switch (roleMap.get(roles, msg.caller)) {
      case (?#Admin) "Admin action executed.";
      case _ throw Error.reject("Access denied: Admins only.");
    };
  };

  // Member-only function
  public shared(msg) func memberAction() : async Text {
    switch (roleMap.get(roles, msg.caller)) {
      case (?#Admin) "Admins can also perform member actions.";
      case (?#Member) "Member action executed.";
      case _ throw Error.reject("Access denied: Members only.");
    };
  };
}
```

### Mandatory access control

Mandatory access control (MAC) enforces strict, system-defined rules that users cannot modify. Unlike discretionary access control (DAC), where the owner can grant and revoke permissions, MAC establishes fixed access policies that remain immutable after deployment. This model is useful for high-security applications, where access rules must be enforced without user intervention.

```motoko no-repl
import Principal "mo:base/Principal";
import OrderedMap "mo:base/OrderedMap";
import Error "mo:base/Error";

shared(msg) persistent actor class MandatoryAccess() {
  let controller = msg.caller;

  type AccessLevel = { #ReadOnly; #WriteOnly; #ReadWrite };

  // Create an OrderedMap instance for storing fixed access rules
  transient let accessMap = OrderedMap.Make<Principal>(Principal.compare);
  stable var accessLevels : OrderedMap.Map<Principal, AccessLevel> = accessMap.empty();

  // Ensure the controller always has ReadWrite access
  system func postupgrade() {
    if (accessMap.get(accessLevels, controller) == null) {
      accessLevels := accessMap.put(accessLevels, controller, #ReadWrite);
    };
  };

  // Function to check access level
  public shared query(msg) func getAccessLevel() : async ?AccessLevel {
    if (msg.caller == controller) return ?#ReadWrite;
    accessMap.get(accessLevels, msg.caller);
  };

  // Read function (accessible by RW and R users)
  public shared composite query(msg) func readData() : async Text {
    switch (await getAccessLevel()) {
      case (?#ReadOnly) "Read access granted.";
      case (?#ReadWrite) "Read access granted.";
      case _ throw Error.reject("Access denied: No read permissions.");
    };
  };

  // Write function (accessible by RW and W users)
  public shared composite query(msg) func writeData(newData : Text) : async Text {
    switch (await getAccessLevel()) {
      case (?#WriteOnly) "Data written: " # newData;
      case (?#ReadWrite) "Data written: " # newData;
      case _ throw Error.reject("Access denied: No write permissions.");
    };
  };
}
```

### Access control list

An access control list (ACL) defines explicit per-user permissions for specific actions, allowing fine-grained control over function access. Unlike role-based access control (RBAC) which groups users into roles, ACLs require manual management of user access for each function. ACLs are flexible but can become difficult to scale in larger applications.

The following example implements separate read and write allow lists, where the controller manages permissions by adding or removing users.

```motoko no-repl
import Principal "mo:base/Principal";
import Map "mo:base/OrderedMap";
import Error "mo:base/Error";

shared(msg) persistent actor class ACLAccess() {
  let controller = msg.caller;

  // Create map instances for separate allow lists
  transient let aclMap = Map.Make<Principal>(Principal.compare);
  stable var readAccessList : Map.Map<Principal, Bool> = aclMap.empty();
  stable var writeAccessList : Map.Map<Principal, Bool> = aclMap.empty();

  // Add a user to the read ACL
  public shared(msg) func grantReadAccess(user : Principal) : async () {
    if (msg.caller != controller) {
      throw Error.reject("Unauthorized: Only the controller can modify the ACL.");
    };
    readAccessList := aclMap.put(readAccessList, user, true);
  };

  // Add a user to the write ACL
  public shared(msg) func grantWriteAccess(user : Principal) : async () {
    if (msg.caller != controller) {
      throw Error.reject("Unauthorized: Only the controller can modify the ACL.");
    };
    writeAccessList := aclMap.put(writeAccessList, user, true);
  };

  // Remove a user from the read ACL
  public shared(msg) func revokeReadAccess(user : Principal) : async () {
    if (msg.caller != controller) {
      throw Error.reject("Unauthorized: Only the controller can modify the ACL.");
    };
    readAccessList := aclMap.delete(readAccessList, user);
  };

  // Remove a user from the write ACL
  public shared(msg) func revokeWriteAccess(user : Principal) : async () {
    if (msg.caller != controller) {
      throw Error.reject("Unauthorized: Only the controller can modify the ACL.");
    };
    writeAccessList := aclMap.delete(writeAccessList, user);
  };

  // Check if a user has read access
  public shared query(msg) func hasReadAccess() : async Bool {
    aclMap.get(readAccessList, msg.caller) == ?true;
  };

  // Check if a user has write access
  public shared query(msg) func hasWriteAccess() : async Bool {
    aclMap.get(writeAccessList, msg.caller) == ?true;
  };

  // Read function (restricted by read ACL)
  public shared composite query(msg) func readData() : async Text {
    if (not (await hasReadAccess())) {
      throw Error.reject("Access denied: Read access required.");
    };
    "Read access granted.";
  };

  // Write function (restricted by write ACL)
  public shared composite query(msg) func writeData(newData : Text) : async Text {
    if (not (await hasWriteAccess())) {
      throw Error.reject("Access denied: Write access required.");
    };
    "Data written: " # newData;
  };
}
```

### Attribute-based access control

Attribute-based access control (ABAC) grants permissions based on user attributes rather than predefined roles or explicit allow lists. In decentralized applications, attributes can include token balances, reputation scores, or other onchain data, making ABAC ideal for Web3 environments.

This example implements a token-gated access model where users must hold a minimum number of tokens to access restricted functions.

```motoko no-repl
import Principal "mo:base/Principal";
import Map "mo:base/OrderedMap";
import Error "mo:base/Error";
import Nat "mo:base/Nat";

shared(msg) persistent actor class AttributeBasedAccess(tokenThreshold : Nat) {
  let controller = msg.caller;

  // Create an OrderedMap instance to track user balances
  transient let balanceMap = Map.Make<Principal>(Principal.compare);
  stable var balances : Map.Map<Principal, Nat> = balanceMap.empty();

  // Function to set a user's balance (simulating token ownership)
  public shared(msg) func setBalance(user : Principal, amount : Nat) : async () {
    if (msg.caller != controller) {
      throw Error.reject("Unauthorized: Only the controller can modify balances.");
    };
    balances := balanceMap.put(balances, user, amount);
  };

  // Function to check if a user meets the access requirement
  public shared query(msg) func hasAccess() : async Bool {
    switch (balanceMap.get(balances, msg.caller)) {
      case (?balance) balance >= tokenThreshold;
      case _ false;
    }
  };

  // Restricted function accessible only to users meeting the threshold
  public shared composite query(msg) func restrictedAction() : async Text {
    if (not (await hasAccess())) {
      throw Error.reject("Access denied: Insufficient tokens.");
    };
    "Access granted: You meet the token requirement.";
  };
}
```

### Policy-based access control

Policy-based access control (PBAC) enforces access rules dynamically based on contextual conditions rather than predefined roles or attributes. Policies can include time-based restrictions, geographical constraints, or  multi-factor conditions. This model is useful for governance, scheduled access, and automated compliance rules.

The following example implements a time-based policy, where access is only granted within a specific time window (9AM- 5PM UTC).

```motoko no-repl
import Error "mo:base/Error";
import Int "mo:base/Int";
import Time "mo:base/Time";
import Nat "mo:base/Nat";

shared(msg) actor class PolicyBasedAccess() {

  // Convert hours to nanoseconds (Time.now() returns nanoseconds)
  private func hoursToNanos(hours : Nat) : Int {
    return Int.abs(hours * 3600 * 1000 * 1000 * 1000);
  };

  // Function to check if the current time is within working hours (9am-5pm)
  public shared query func isAccessAllowed() : async Bool {
    let currentTime = Time.now();

    // Calculate seconds since midnight in UTC
    let nanosecondsPerDay : Int = 24 * 3600 * 1000 * 1000 * 1000;
    let daysSinceEpoch : Int = currentTime / nanosecondsPerDay;
    let nanosecondsSinceMidnight : Int = currentTime - (daysSinceEpoch * nanosecondsPerDay);

    // Define working hours in nanoseconds (9am - 5pm)
    let workStartTime = hoursToNanos(9);
    let workEndTime = hoursToNanos(17); // 5pm in 24-hour format

    // Check if current time is within working hours
    return nanosecondsSinceMidnight >= workStartTime and nanosecondsSinceMidnight <= workEndTime;
  };

  // Restricted function that only allows access within working hours
  public shared composite query func restrictedAction() : async Text {
    if (not (await isAccessAllowed())) {
      throw Error.reject("Access denied: Outside working hours (9am-5pm).");
    };
    "Access granted: You are within working hours (9am-5pm).";
  };

  // For testing: returns current time info
  public query func getTimeInfo() : async Text {
    let currentTime = Time.now();
    let nanosecondsPerDay : Int = 24 * 3600 * 1000 * 1000 * 1000;
    let daysSinceEpoch : Int = currentTime / nanosecondsPerDay;
    let nanosecondsSinceMidnight : Int = currentTime - (daysSinceEpoch * nanosecondsPerDay);
    let hoursSinceMidnight : Int = nanosecondsSinceMidnight / (3600 * 1000 * 1000 * 1000);

    return "Current time: " # Int.toText(currentTime) #
           "\nHours since midnight (UTC): " # Int.toText(hoursSinceMidnight);
  };
}
```

## Resources

- [`Principal`](https://internetcomputer.org/docs/motoko/base/Principal)
- [`Principal` specification](https://internetcomputer.org/docs/references/ic-interface-spec/#principal)
