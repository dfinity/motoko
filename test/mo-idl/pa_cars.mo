import Car "import/car";

actor class (dmv : Car.DMV) {
  public func verifyCarInformation(user : User, car : Car.Car) : async ?(shared (Location, TimeSpan) -> async Result) {
    let carInfo = await dmv.check(car);
    if (carInfo.isValid and not carInfo.wasStolen) {
      return ?(shared func (location:Location, time:TimeSpan) : async Result {
        return reserveSpot(user, carInfo, location, time);
      })
    } else {
      return null;
    }
  };

  flexible func reserveSpot(user : User, carInfo : Car.CarInfo, location : Location, timeSpan : TimeSpan) : Result {
    // Do the actual work of registering the parking spot for the
    // given car in the given time span
    return null;
  };

  public type Location = { lat : Float; long : Float };
  public type TimeSpan = { start : Int; end : Int };
  public type Result = ?({ reservationId : Text });
  public type User = { name : Text };
};
