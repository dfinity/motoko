import Prim "mo:prim";
{
Prim.debugPrint("Zero byte: >\00<");
};
{
Prim.debugPrint("FF byte: >\ff<");
};
{
Prim.debugPrint("uFF char: >\u{ff}<");
};
