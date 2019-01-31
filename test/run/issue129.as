// status.as
type Status = {
  failed_ : Nat;
  passed_ : Nat;
  pending_ : Nat;
};

let appendStatus = func (x : Status, y : Status) : Status {
  new {
    failed_ = x.failed_ + y.failed_;
    passed_ = x.passed_ + y.passed_;
    pending_ = x.pending_ + y.pending_;
  };
};