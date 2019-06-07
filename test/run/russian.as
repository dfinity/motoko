let russian = "Приветствую, мир!\n";
let (len, c) = decodeUTF8 russian;
assert (len == (2 : Word32));
assert (c == 'П');
assert (charToWord32 c == (1055 : Word32));


{
    let (len, c) = decodeUTF8 russian;
    assert ((len == (2 : Word32)) and (c == 'П'));
};

{
    let emojis = "🙈🎸😋";
    let (len, c) = decodeUTF8 emojis;
    assert ((len == (4 : Word32)) and (c == '\u{1f648}'));
    assert(emojis.len() == 3);
};

assert(russian.len() == 18);

var x = 0;
for (a in russian.chars()) {
  x += 1;
  switch x {
    case 1 { assert (a == 'П') };
    case 2 { assert (a == 'р') };
    case _ { }
  }
};

assert(x == 18);
