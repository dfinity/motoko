type Main = actor { beep : () -> async () };

// let-like
actor Main : Main = {

};

// less attractive
actor Main2 {

} : Main
