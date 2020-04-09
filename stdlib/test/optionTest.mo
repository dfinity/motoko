import Option "mo:stdlib/Option";
import Prelude "mo:stdlib/Prelude";

Prelude.printLn("Option");

{
  Prelude.printLn("  apply");

  {
    Prelude.printLn("    null function, null value");

    let actual = Option.apply<Int, Bool>(null, null);
    let expected : ?Bool = null;

    switch (actual, expected) {
      case (?actual_, ?expected_) {
        assert(false);
      };
      case (_, _) {
        assert(true);
      };
    };
  };

  {
    Prelude.printLn("    null function, non-null value");

     let actual = Option.apply<Int, Bool>(null, ?0);
    let expected : ?Bool = null;

     switch (actual, expected) {
      case (?actual_, ?expected_) {
        assert(false);
      };
      case (_, _) {
        assert(true);
      };
    };
  };

  {
    Prelude.printLn("    non-null function, null value");

     let isEven = func (x : Int) : Bool {
      x % 2 == 0;
    };

    let actual = Option.apply<Int, Bool>(?isEven, null);
    let expected : ?Bool = null;

    switch (actual, expected) {
      case (?actual_, ?expected_) {
        assert(false);
      };
      case (_, _) {
        assert(true);
      };
    };
  };

  {
    Prelude.printLn("    non-null function, non-null value");

   let isEven = func (x : Int) : Bool {
      x % 2 == 0;
    };

    let actual = Option.apply<Int, Bool>(?isEven, ?0);
    let expected = ?true;

    switch (actual, expected) {
      case (?actual_, ?expected_) {
        assert(actual_ == expected_);
      };
      case (_, _) {
        assert(false);
      };
    };
  };

 };

{
  Prelude.printLn("  bind");

  {
    Prelude.printLn("    null value to null value");

    let safeInt = func (x : Int) : ?Int {
      if (x > 9007199254740991) {
        null;
      } else {
        ?x;
      }
    };

    let actual = Option.bind<Int, Int>(null, safeInt);
    let expected : ?Int = null;

    switch (actual, expected) {
      case (?actual_, ?expected_) {
        assert(false);
      };
      case (_, _) {
        assert(true);
      };
    };
  };

  {
    Prelude.printLn("    non-null value to null value");

    let safeInt = func (x : Int) : ?Int {
      if (x > 9007199254740991) {
        null;
      } else {
        ?x;
      }
    };

    let actual = Option.bind<Int, Int>(?9007199254740992, safeInt);
    let expected : ?Int = null;

    switch (actual, expected) {
      case (?actual_, ?expected_) {
        assert(false);
      };
      case (_, _) {
        assert(true);
      };
    };
  };

  {
    Prelude.printLn("    non-null value to non-null value");

    let safeInt = func (x : Int) : ?Int {
      if (x > 9007199254740991) {
        null;
      } else {
        ?x;
      }
    };

    let actual = Option.bind<Int, Int>(?0, safeInt);
    let expected = ?0;

    switch (actual, expected) {
      case (?actual_, ?expected_) {
        assert(actual_ == expected_);
      };
      case (_, _) {
        assert(false);
      };
    };
  };

};

{
  Prelude.printLn("  join");

  {
    Prelude.printLn("    null value");

    let actual = Option.join<Int>(?null);
    let expected : ?Int = null;

    switch (actual, expected) {
      case (?actual_, ?expected_) {
        assert(false);
      };
      case (_, _) {
        assert(true);
      };
    };
  };

  {
    Prelude.printLn("    non-null value");
    let actual = Option.join<Int>(??0);
    let expected = ?0;

     switch (actual, expected) {
      case (?actual_, ?expected_) {
        assert(actual_ == expected_);
      };
      case (_, _) {
        assert(false);
      };
    };
  };

};

{
  Prelude.printLn("  map");

  {
    Prelude.printLn("    null value");

    let isEven = func (x : Int) : Bool {
      x % 2 == 0;
    };

    let actual = Option.map<Int, Bool>(isEven, null);
    let expected : ?Bool = null;

    switch (actual, expected) {
      case (?actual_, ?expected_) {
        assert(false);
      };
      case (_, _) {
        assert(true);
      };
    };
  };

  {
    Prelude.printLn("    non-null value");

    let isEven = func (x : Int) : Bool {
      x % 2 == 0;
    };

    let actual = Option.map<Int, Bool>(isEven, ?0);
    let expected = ?true;

    switch (actual, expected) {
      case (?actual_, ?expected_) {
        assert(actual_ == expected_);
      };
      case (_, _) {
        assert(false);
      };
    };
  };

};

{
  Prelude.printLn("  pure");

  let actual = Option.pure<Int>(0);
  let expected = ?0;

  switch (actual, expected) {
    case (?actual_, ?expected_) {
      assert(actual_ == expected_);
    };
    case (_, _) {
      assert(false);
    };
  };
};
