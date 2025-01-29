---
sidebar_position: 23
---

# Sharing data and behavior



In Motoko, mutable state is always private to an actor. However, two actors can share message data, and those messages can refer to actors, including themselves and one another. Additionally, messages can refer to individual functions, if those functions are `shared`.

Through these mechanisms, two actors can coordinate their behavior through asynchronous message passing.

## Publisher-subscriber pattern with actors

The examples in this section illustrate how actors share their functions by focusing on variations of the [publish-subscribe pattern](https://en.wikipedia.org/wiki/Publish-subscribe_pattern). In the publish-subscribe pattern, a **publishing** actor records a list of **subscriber** actors to notify when something notable occurs in the publisher’s state. For example, if the publisher actor publishes a new article, the subscriber actors are notified that a new article is available.

The example below uses two actors in Motoko to build variations of the publisher-subscriber relationship.

To see the complete code for a working project that uses this pattern, see the [pubsub](https://github.com/dfinity/examples/tree/master/motoko/pubsub) example in the [examples repository](https://github.com/dfinity/examples).

### Subscriber actor

The following `Subscriber` actor type provides a possible interface for the subscriber actor to expose and the publisher actor to call:

``` motoko name=tsub
type Subscriber = actor {
  notify : () -> ()
};
```

-   The `Publisher` uses this type to define a data structure to store its subscribers as data.

-   Each `Subscriber` actor exposes a `notify` update function as described in the `Subscriber` actor type signature above.

Note that sub-typing enables the `Subscriber` actor to include additional methods that are not listed in this type definition.

For simplicity, assume that the `notify` function accepts relevant notification data and returns some new status message about the subscriber to the publisher. For example, the subscriber might return a change to its subscription settings based on the notification data.

### Publisher actor

The publisher side of the code stores an array of subscribers. For simplicity, assume that each subscriber only subscribes itself once using a `subscribe` function:

``` motoko no-repl
import Array "mo:base/Array";

persistent actor Publisher {

  var subs : [Subscriber] = [];

  public func subscribe(sub : Subscriber) {
    subs := Array.append<Subscriber>(subs, [sub]);
  };

  public func publish() {
    for (sub in subs.vals()) {
      sub.notify();
    };
  };
}
```

Later, when some unspecified external agent invokes the `publish` function, all of the subscribers receive the `notify` message as defined in the `Subscriber` type given above.

### Subscriber methods

In the simplest case, the subscriber actor has the following methods:

-   Subscribe to notifications from the publisher using the `init` method.

-   Receive notification as one of the subscribed actors, as specified by the `notify` function in the `Subscriber` type given above.

-   Permit queries to the accumulated state, which in this sample code is simply a `get` method for the number of notifications received and stored in the `count` variable.

The following code illustrates implementing these methods:

``` motoko no-repl
persistent actor Subscriber {

  var count : Nat = 0;

  public func init() {
    Publisher.subscribe(Subscriber);
  };

  public func notify() {
    count += 1;
  };

  public func get() : async Nat {
    count
  };
}
```

The actor assumes, but does not enforce, that its `init` function is only ever called once. In the `init` function, the `Subscriber` actor passes a reference to itself of type `actor { notify : () -> () };`.

If called more than once, the actor will subscribe itself multiple times and will receive multiple duplicate notifications from the publisher. This fragility is the consequence of the basic publisher-subscriber design shown above. A more advanced publisher actor could check for duplicate subscriber actors and ignore them.

## Sharing functions among actors

In Motoko, a `shared` actor function can be sent in a message to another actor and then later called by that actor or by another actor.

The code shown above has been simplified for illustrative purposes. The full version offers additional features to the publisher-subscriber relationship, and uses shared functions to make this relationship more flexible.

For instance, the notification function is always designated as `notify`. A more flexible design would only fix the type of `notify`, and permit the subscriber to choose any of its `shared` functions.

See the [the full example](https://github.com/dfinity/examples/tree/master/motoko/pub-sub) for details.

In particular, suppose that the subscriber wants to avoid being locked into a certain naming scheme for its interface. What really matters is that the publisher can call some function that the subscriber chooses.

### The `shared` keyword

To permit this flexibility, an actor needs to share a single function that permits remote invocation from another actor, not merely a reference to itself.

The ability to share a function requires that it be pre-designated as `shared` and the type system enforces that these functions follow certain rules around the types of data that these functions accept as arguments and return as result. In particular, the data that can be transmitted across shared functions must have a shared type consisting of immutable plain data, actor references or references to shared functions. Local functions, proper objects with methods and mutable arrays are excluded.

Motoko lets you omit this keyword for public actor methods since implicitly, any public function of an actor must be `shared`, whether marked explicitly or not.

Using the `shared` function type, we can extend the example above to be more flexible. For example:

``` motoko
type SubscribeMessage = { callback : shared () -> (); };
```

This type differs from the original in that it describes a message record type with a single field called `callback`. The original type first shown above describes an actor type with a single method called `notify`:

``` motoko
type Subscriber = actor { notify : () -> () };
```

Notably, the `actor` keyword means that this latter type is not an ordinary record with fields but rather an actor with at least one method, which must be called `notify`.

By using the `SubscribeMessage` type instead, the `Subscriber` actor can choose another name for their `notify` method:

``` motoko no-repl
persistent actor Subscriber {

  var count : Nat = 0;

  public func init() {
    Publisher.subscribe({callback = incr;});
  };

  public func incr() {
    count += 1;
  };

  public query func get(): async Nat {
    count
  };
}
```

Compared to the original version, the only lines that change are those that rename `notify` to `incr`, and form the new `subscribe` message payload using the expression `{callback = incr}`.

Likewise, we can update the publisher to have a matching interface:

``` motoko no-repl
import Array "mo:base/Array";

persistent actor Publisher {

  var subs : [SubscribeMessage] = [];

  public func subscribe(sub : SubscribeMessage) {
    subs := Array.append<SubscribeMessage>(subs, [sub]);
  };

  public func publish() {
    for (sub in subs.vals()) {
      sub.callback();
    };
  };
}
```

<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />