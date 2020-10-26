type Tree<T> = {
  #leaf : T;
  #branch : {left : Tree<T>; right : Tree<T>};
};

func iterTree<T>(tree : Tree<T>, f : T -> ()) {
  switch (tree) {
    case (#leaf(x)) { f(x) };
    case (#branch{left; right}) {
      iterTree(left, f);
      iterTree(right, f);
    };
  }
}
