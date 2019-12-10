import H "Hashtbl.mo";
import Hash "Hash.mo";

func textIsEq(x:Text,y:Text):Bool { x == y };

let a = H.Hashtbl<Text, Nat>(3, textIsEq, Hash.Hash.hashOfText);

ignore a.set("apple", 1);
ignore a.set("banana", 2);
ignore a.set("pear", 3);
ignore a.set("avocado", 4);

for ((k,v) in a.iter()) {
  debugPrint(debug_show (k,v));
};

/*
prints these pairs, in some order or another:
("banana", 2)
("pear", 3)
("apple", 1)
("avocado", 4)
*/
