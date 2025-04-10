import Float "mo:base/Float";

module Migration {

  public func migration(old: { var state : Int }) : { var state : Float } {
    { var state = Float.fromInt(old.state) };
  }

}
