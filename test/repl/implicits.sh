#!/usr/bin/env bash
# Tests inference and printing of implicit argument types
moc -i <<__END__
func f (implicit a : Nat, // simple
        implicit b : Nat = b1, // override
        implicit _ : Nat = c, // wildcard
        d : (implicit : Nat), // legacy
        e1 : (implicit : (e : Nat)), //legacy override
        f : (implicit : (_ : Nat)), // legacy wildcard
        _ : (implicit : (g : Nat)), // legacy anon
        _ : (implicit : (_ : Nat)), // legacy wildcard anon
        ) {};

func u1 (implicit a : Nat, // simple
        ) {};

func u2 (implicit b : Nat = b1, // override
        ) {};

func u3 (implicit _ : Nat = c, // wildcard
        ) {};

func u4 (d : (implicit : Nat), // legacy
        ) {};

func u5 (e1 : (implicit : (e : Nat)), //legacy override
        ) {};

func u6 (f : (implicit : (_ : Nat)), // legacy wildcard
        ) {};

func u7 (_ : (implicit : (g : Nat)), // legacy anon
        ) {};

func u8 (_ : (implicit : (_ : Nat)), // legacy wildcard anon
        ) {};


__END__
