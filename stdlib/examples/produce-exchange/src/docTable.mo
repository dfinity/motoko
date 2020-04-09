import Hash "mo:stdlib/Hash";

import Trie "mo:stdlib/Trie";

module {
/**

Document Table
===============

This table abstracts over a set of _documents_, each with a distinct
id assigned by this abstraction.

Documents potentially contain _deep nested structure_, e.g., other
document collections, etc.

Each document has a shallow, lossy projection to its _document
information_; this information may contain more than a unique id, but
is sufficiently concise to transmit in a server-to-client message.
Likewise, document information seeds a new document, e.g., in a
client-to-server message with this _initial document information_.

See the [client interface](#client-interface) below for detailed
type information.

*/

/**
 Representation
 ================
 A table is a finite map (currently a [Trie]($DOCURL/trie.html)) mapping ids to documents.

 See also: [private state](#private-state).

 notes on representation
 -------------------------

 The Motoko standard library provides several purely-functional finite map representations:

 - as association lists (via modules `List` and `AssocList`)
 - and  as hash tries (via (module `Trie`), whose representation uses those lists, for its
 "buckets".

 These map representations could change and expand in the future, so we
 introduce the name `Table` here to abstract over the representation
 choice between (for now) using tries (and internally, association lists).

 */

public type Hash = Hash.Hash;
public type Trie<K,V> = Trie.Trie<K,V>;
public type Key<K> = Trie.Key<K>;

/**
 We choose to represent each `Table` as a `Trie`:
*/

public let Table = Trie;
public type Table<Id, Doc> = Trie<Id, Doc>;

/**

 Aside: Eventually, we'll likely have a more optimized trie that uses
 small arrays in its leaf nodes.  The current representation is simple,
 uses lots of pointers, and is likely not the optimal candidate for
 efficient Wasm.  However, its asymptotic behavior is good, and it thus
 provides a good approximation of the eventual design that we want.

*/

/**
 Client interface
 ===============================

 When the client provides the [parameters below](#client-parameters),
this module [implements the public interface given further
below](#public-interface).

 */

/**
 Client parameters
 ==================

 The document table abstracts over the following client choices:

 - types `Id`, `Doc` and `Info`.
 - `idFirst,` -- the first id to use in the generation of distinct ids.
 - `idIncr` -- increment function for ids.
 - `idIsEq` -- equality function for ids.
 - `idHash` -- hash function for ids.
 - `infoOfDoc` -- project the document information from a document.
 - `docOfInfo` -- seed and validate client-provided document information.

 See the types below for details.

 */
public class DocTable<Id,Doc,Info>(
  idFirst:Id,
  idIncr:Id->Id,
  idIsEq:(Id,Id)->Bool,
  idHash:Id->Hash,
  infoOfDoc:Doc->Info,
  docOfInfo:Info->?Doc
) = this {

/**
 Public interface
 ===============================
*/

  /**
   `empty`
   ---------

   See also [`Table.empty`]($DOCURL/trie.md#empty)

   */

  public func empty() : Table<Id, Doc> {
    Table.empty<Id, Doc>()
  };

  /**
   `reset`
   --------

   Reset the document table back to its initial counter, and empty state.
   */
  public func reset() {
    idNext := idFirst;
    table := Table.empty<Id,Doc>();
  };

  /**
   `getTable`
   ---------

   See also [`Table.copy`]($DOCURL/trie.md#copy)

   */

  public func getTable() : Table<Id, Doc> {
    Table.copy<Id, Doc>(table)
  };

  /**
   `addDoc`
   ---------

   See also [`Table.insertFresh`]($DOCURL/trie.md#insertfresh)

   */

  public func addDoc(doc:Id -> Doc) : (Id, Doc) {
    let id = idNext;
    idNext := idIncr(idNext);
    let d = doc(id);
    table := Table.insertFresh<Id, Doc>
    (table, keyOfId(id), idIsEq, d);
    (id, d)
  };

  /**
   `updateDoc`
   ---------

   See also [`Table.replace`]($DOCURL/trie.md#insertfresh)

   */

  public func updateDoc(id:Id, doc:Doc) : ?Doc {
    let (updatedTable, oldDoc) = Table.replace<Id, Doc>
    (table, keyOfId(id), idIsEq, ?doc);
    table := updatedTable;
    oldDoc
  };

  /**
   `addInfoAs`
   ---------

   See also [`addInfo`](#addinfo).

   This variant of `addInfo` permits the caller to choose the id, but still insists that it be fresh (not currently in use).

   */
  public func addInfoAs(idChoice:?Id, info:Id -> Info) : ?(Id, Doc) {
    switch idChoice {
      // subcase: No pre-chosen Id, so mint a new fresh one:
      case null {
             let id = idNext;
             let doc = docOfInfo(info(id));
             switch doc {
             case null { null };
             case (?doc) {
                    idNext := idIncr(idNext);
                    table := Table.insertFresh<Id, Doc>
                    (table, keyOfId(id), idIsEq, doc);
                    ?(id, doc)
                  }
             }
           };
      // subcase: Have a pre-chosen Id, so use that; still must be fresh.
      case (?idChoice_) {
             let doc = docOfInfo(info(idChoice_));
             switch doc {
             case null { null };
             case (?doc) {
                    table := Table.insertFresh<Id, Doc>
                    (table, keyOfId(idChoice_), idIsEq, doc);
                    ?(idChoice_, doc)
                  }
             }
           };
      }
  };

  /**
   `addInfo`
   ---------

   See also [`Table.insertFresh`]($DOCURL/trie.md#insertfresh)

   */
  public func addInfo(info:Id -> Info) : ?(Id, Doc) {
    addInfoAs(null, info)
  };

  public func addInfoGetId(info:Id -> Info) : ?Id {
    switch (addInfo(info)) {
      case null { null };
      case (?(id, doc)) { ?id }
    }
  };

  /**
   `rem`
   ---------

   See also [`Table.removeThen`]($DOCURL/trie.md#removeThen)

   */

  public func rem(id:Id) : ?Doc {
    Table.removeThen<Id, Doc, ?Doc>(
      table, keyOfId(id), idIsEq,
      func (t:Table<Id, Doc>, d:Doc) : ?Doc {
        table := t;
        ?d
      },
      func ():?Doc = null
    )
  };


  public func remGetId(id:Id) : ?Id {
    Table.removeThen<Id, Doc, ?Id>(
      table, keyOfId(id), idIsEq,
      func (t:Table<Id, Doc>, d:Doc) : ?Id {
        table := t;
        ?id
      },
      func ():?Id = null
    )
  };

  public func remGetUnit(id:Id) : ?() {
    Table.removeThen<Id, Doc, ?()>(
      table, keyOfId(id), idIsEq,
      func (t:Table<Id, Doc>, d:Doc) : ?() {
        table := t;
        ?()
      },
      func ():?() = null
    )
  };

  /**
   `getDoc`
   ---------

   See also [`Table.find`]($DOCURL/trie.md#find)

   */

  public func getDoc(id:Id) : ?Doc {
    Table.find<Id, Doc>(table, keyOfId(id), idIsEq)
  };

  /**
   `getInfo`
   ---------
   */

  public func getInfo(id:Id) : ?Info {
    switch (getDoc(id)) {
      case null null;
      case (?doc) { ?infoOfDoc(doc) };
    }
  };

  /**
   `count`
   ---------

   See also [`Table.count`]($DOCURL/trie.md#count)
  */

  public func count() : Nat {
    Table.count<Id, Doc>(table)
  };

  /**
   `allDoc`
   ---------

   See also [`Table.toArray`]($DOCURL/trie.md#toarray)
   */

  public func allDoc() : [Doc] {
    Table.toArray<Id, Doc, Doc>
    (table, func (id:Id, doc:Doc):Doc = doc )
  };

  /**
   `allInfo`
   ---------

   See also [`Table.toArray`]($DOCURL/trie.md#toarray)
  */

  public func allInfo() : [Info] {
    Table.toArray<Id, Doc, Info>
    (table, func (id:Id, doc:Doc):Info = infoOfDoc(doc) )
  };


/**
 Public helpers
 ===============
 */

  public func keyOfId(x:Id) : Key<Id>     = { key = x ; hash = idHash(x) };

  public func getIdIsEq() :(Id,Id)->Bool  = idIsEq;
  public func getIdHash() : Id->Hash      = idHash;

  public func getInfoOfDoc() : Doc->Info  = infoOfDoc;
  public func getDocOfInfo() : Info->?Doc = docOfInfo;


/**
 Private state
 ===============
 */

  var idNext:Id = idFirst;

  var table : Table<Id,Doc> = Table.empty<Id,Doc>();

/**
 Helpers
 ===============
 */


/** The end */

}
}
