type Data = { a : Nat }; // forgot about field b
type OldActor = { data : [Data] };
type NewActor = { data : [Data] };
(
  with migration = func(r : OldActor) : NewActor { r }
)
persistent actor {
  type Data = { a : Nat; b : Text };
  let data : [Data] = [{ a = 0; b = "" }];
};
