#!/usr/bin/env bash
# Tests inference of named types
moc -i <<__END__
func (#tag (f:Nat)){};
func (#tag (_:Nat)){};
func ({x : Nat}){};
func ({x = i : Nat}) {};
func ((?x):?Nat) {};
func (x : Nat) {};
func (x : Nat, y : Nat, z : Nat) {};
func (?(x : Nat, y : Nat, z : Nat)) {};
func (?(x : Nat)) {};
__END__
