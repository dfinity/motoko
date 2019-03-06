let _ = (actor this {}) : Any;
let _ = (actor {}) : Any;


let _ = (object this {}) : Any;
let _ = (object {}) : Any;


let _ = (actor this { func x(){ this.x(); }; }) : Any;
let _ = (actor { func x(){x();}; }) : Any;


let _ = (object this { func x(){ this.x(); }; }) : Any;
let _ = (object { func x(){x();}; }) : Any;


