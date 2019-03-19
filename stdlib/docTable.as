
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

This table abstracts over the following choices, which it accepts as
parameters:

 - `idFirst,` -- the first id to use in the generation of distinct ids.
 - `idIncr` -- increment function for ids.
 - `idIsEq` -- equality function for ids.
 - `idHash` -- hash function for ids.
 - `infoOfDoc` -- project the document information from a document.
 - `docOfInfo` -- seed and validate client-provided document information.

*/


/**
 Representation
 ---------------
 A table is a finite map (currently a trie) mapping ids to documents.
 */
type Table<K,V> = Trie<K,V>;
let Table = Trie;

/**
 Interface and implementation
 -----------------------------
 */

class DocTable<Id,Doc,Info>(
  idFirst:Id,
  idIncr:Id->Id,
  idIsEq:(Id,Id)->Bool,
  idHash:Id->Hash,
  infoOfDoc:Doc->Info,
  docOfInfo:Info->?Doc
) = this {

  /** 
   `addDoc`
   ---------
   */

  addDoc(doc:Id -> Doc) : Id {
    let id = idNext;
    idNext := idIncr(idNext);
    table := Table.insertFresh<Id, Doc>
    (table, keyOf(id), idIsEq, doc(id));
    id    
  };

  /** 
   `addInfo`
   ---------
   */

  addInfo(info:Id -> Info) : ?Id {
    let id = idNext;
    switch (docOfInfo(info(id))) {
      case null { null };
      case (?doc) {
             idNext := idIncr(idNext);
             table := Table.insertFresh<Id, Doc>
             (table, keyOf(id), idIsEq, doc);
             ?id
           }
    }
  };

  /** 
   `rem`
   ---------
   */

  rem(id:Id) : ?Doc {
    Table.removeThen<Id, Doc, ?Doc>(
      table, keyOf(id), idIsEq,
      func (t:Table<Id, Doc>, d:Doc) : ?Doc {
        table := t;
        ?d
      },
      func ():?Doc = null
    )
  };

  /** 
   `getDoc`
   ---------
   */
  
  getDoc(id:Id) : ?Doc {
    Table.find<Id, Doc>(table, keyOf(id), idIsEq)
  };

  /** 
   `getInfo`
   ---------
   */

  getInfo(id:Id) : ?Info {
    switch (getDoc(id)) {
      case null null;
      case (?doc) { ?infoOfDoc(doc) };
    }
  };

  /** 
   `allDoc`
   ---------
   */

  allDoc() : [Doc] {
    // todo need to implement an array-append operation, then use trie foldUp
    // xxx    
    []
  };

  /** 
   `allInfo`
   ---------
   */

  allInfo() : [Info] {
    // todo need to implement an array-append operation, then use trie foldUp
    // xxx    
    []
  };

//@Omit:

  private var idNext:Id = idFirst;
  
  private var table : Table<Id,Doc> = null;

  private keyOf(x:Id):Key<Id> {
    new { key = x ; hash = idHash(x) }
  };

/** The end */

}
  
