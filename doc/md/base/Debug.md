# Debug
Debugging aids

## Value `print`
``` motoko no-repl
let print : Text -> ()
```

`print(t)` emits text `t` to the debug output stream.
How this stream is stored or displayed depends on the
execution environment.

## Value `trap`
``` motoko no-repl
let trap : Text -> None
```

`trap(t)` traps execution with a user-provided message.
