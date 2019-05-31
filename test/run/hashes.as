
assert (hashInt (10**7) == (10000000 : Word32));
assert (hashInt 0 == (0 : Word32));
assert (hashInt (10**18) == (2_860_824_243 : Word32));

assert (hashInt (-1) == (-2 : Word32));
assert (hashInt (-387) == (-388 : Word32));
assert (hashInt (-3876548352991) == (1807116198 : Word32));
