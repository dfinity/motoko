actor foo {
  public func transpose (data : [(Int,Text)]) : async {ints: [Int]; txts: [Text]} {
    return {
      ints = Array_tabulate<Int>(data.len(), func (i:Nat) : Int = (data[i].0));
      txts = Array_tabulate<Text>(data.len(), func (i:Nat) : Text = (data[i].1))
    }
  }
};

async {
  let x = await foo.transpose([(1,"Hi"), (2, "Ho")]);
  assert (x.ints[0] == 1);
  assert (x.ints[1] == 2);
  assert (x.txts[0] == "Hi");
  assert (x.txts[1] == "Ho");
}
