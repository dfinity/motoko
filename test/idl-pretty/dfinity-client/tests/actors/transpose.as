actor {
  transpose (data : [(Int,Text)]) : async (shared {ints: [Int]; txts: [Text]}) {
    return (shared {
      ints = Array_tabulate<Int>(data.len(), func (i:Nat) : Int = (data[i].0));
      txts = Array_tabulate<Text>(data.len(), func (i:Nat) : Text = (data[i].1))
    })
  }
}
