assert ((0x5bafecbd : Word32) <>> (4 : Word32) == (0xd5bafecb : Word32));
assert ((0x5bafecbd : Word32) <>> (36 : Word32) == (0xd5bafecb : Word32));

assert ((0x5bbd : Word16) <>> (4 : Word16) == (0xd5bb : Word16));
assert ((0x5bbd : Word16) <>> (20 : Word16) == (0xd5bb : Word16));


assert ((0x56 : Word8) <>> (3 : Word8) == (0xca : Word8));     // 01010110 -> 11001010
assert ((0x56 : Word8) <>> (11 : Word8) == (0xca : Word8));
assert ((0x56 : Word8) <>> (19 : Word8) == (0xca : Word8));


assert ((0x5bafecbd : Word32) <<> (4 : Word32) == (0xbafecbd5 : Word32));
assert ((0x5bafecbd : Word32) <<> (36 : Word32) == (0xbafecbd5 : Word32));

assert ((0x5bbd : Word16) <<> (4 : Word16) == (0xbbd5 : Word16));
assert ((0x5bbd : Word16) <<> (20 : Word16) == (0xbbd5 : Word16));

assert ((0x56 : Word8) <<> (3 : Word8) == (0xb2 : Word8));     // 01010110 -> 10110010
assert ((0x56 : Word8) <<> (11 : Word8) == (0xb2 : Word8));
assert ((0x56 : Word8) <<> (19 : Word8) == (0xb2 : Word8));
