// @verify
// import Array "mo:base/Array";

actor {

    let immut_arr = [42];
    var arr : [var Int] = [var 1, 2];
    var f : Int = 2;
    var count : Int = 42;

    assert:invariant arr.size() == 2;

    public func foo(): async Int {
        // declarations
        var vi_a : [Int] = (([1] : [Int]) : [Int]);
        var vm_a : [var Int] = [var 1, 2];
        let li_a : [Bool] = [false];
        let lm_a : [var Bool] = [var false];

        assert:system vi_a[0] == 1;
        assert:system vm_a[0] == 1;
        assert:system li_a[0] == false;
        assert:system lm_a[0] == false;

        // assignments
        // vi_a = [1, 2]
        vi_a := [1, 2];
        // li_a := [1, 2]; // error
        vm_a := [var 42];
        // lm_a := [1, 2]; // error

        assert:system vi_a[0] != 0;
        assert:system vi_a[0] == 1;
        assert:system vm_a[0] != 1;
        assert:system vm_a[0] == 42;

        return 42;
    };

    public func baz1(): async [Int] {
      let r : [Int] = [1, 2];
      return r;
    };

    private func baz2(): [var Int] {
      let r : [var Int] = [var 1, 2];
      return r;
    };

    private func bar(): Int {
      let x : [Int] = [42, 24];
      let y : [var Bool] = [var false];
      y[0] := true;
      let z : Int = x[0] + x[1];
      return x[1];
    };

    public func bar2(x: [Int]):  async Int {
      assert:func x.size() == 2;
      let y : [var Bool] = [var false];
      y[0] := true;
      let z : Int = x[0] + x[1];
      return x[1];
    };

    public func inc() : async Int {
      arr[0] := arr[0] + 1;
      return arr[0]
    };

    public func len(): async Int {
      return arr.size();
    };

    public func reset(): async Int {
      arr := [var 0, 0];
      return 0;
    }
}
