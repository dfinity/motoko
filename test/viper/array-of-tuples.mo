// @verify
// import Array "mo:base/Array";

actor ArrayOfTuples {

    public func get_tup_arr(): async [(Int, Bool)] {
      let x1 = (42, true);
      let x2 = (0, false);
      return [x1, x2];
    };

    private func set_nested_mut_arr(arr: [var ?(?Int, (Bool, Bool))]) {
      assert arr.size() >= 2;
      assert:return arr.size() >= 2 and
                    arr[0] == (old (arr[0])) and
                    arr[1] == ?(?42, (true, true));
      arr[1] := ?(?42, (true, true));
    };

    public func check_arr(): async () {
      let arr : [var ?(?Int, (Bool, Bool))] = [var null, null];
      set_nested_mut_arr(arr);
      assert:system arr[0] == null;
      assert:system arr[1] == ?(?42, (true, true));
    }
}
