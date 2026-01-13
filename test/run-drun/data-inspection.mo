//ENHANCED-ORTHOGONAL-PERSISTENCE-ONLY

import Prim "mo:prim";

actor Self {
    stable let line = [{ var x = 1; var y = 2 }, { var x = 3; var y = 4 }, { var x = 5; var y = 6 }];
    stable let coloredLine = { line; var color = "blue" };

    type DataInspection = actor {
        __motoko_inspect_data: () -> async Blob;
    };

    func withDataInspection(a : actor {}) : DataInspection {
        actor (debug_show (Prim.principalOfActor(a))) : DataInspection;
    };

    public func test(): async() {
        let self = withDataInspection(Self);
        let data = await self.__motoko_inspect_data();
        Prim.debugPrint("Data length " # debug_show(data.size()));
    };

    public func change() : async () {
        for (point in line.vals()) {
            point.x += 1;
            point.y += 1;
        };
        if (coloredLine.color == "blue") {
            coloredLine.color := "red";
        } else {
            coloredLine.color := "blue";
        };
    };
};

//SKIP run
//SKIP run-low
//SKIP run-ir
//CALL ingress test "DIDL\x00\x00"
//CALL ingress change "DIDL\x00\x00"
//CALL ingress test "DIDL\x00\x00"
