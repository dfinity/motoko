actor class Bank(supply : Int) {
  private issuer = Issuer();
  private reserve = Account(supply);
  getIssuer() : async Issuer { return issuer; };
  getReserve() : async Account {
   return reserve; };
};

actor class Issuer() {
  hasIssued(account : like Account) : async Bool {
    return (account is Account);
  };
};

actor class Account(initialBalance : Int) {
  private var balance : Int = initialBalance;

  getBalance() : async Int {
    return balance;
  };

  split(amount : Int) : async Account {
    balance -= amount;
    return Account(amount);
  };

  join(account : Account) {  // this implicitly asserts that account is Account
    let amount = balance;
    balance := 0;
    account.credit(amount);
  };

  private credit(amount : Int) {
    // private implicitly asserts that caller is own class
    // by implicitly passing the modref as an extra argument
    balance += amount;
  };

  isCompatible(account : like Account) : async Bool {
    return (account is Account);
  };
};

// Example usage
func transfer(sender : Account, receiver : Account, amount : Int) : async /* hack: */ ()  {
  let trx = await sender.split(amount);
  trx.join(receiver);
};


let b = Bank(100);

func show(t:Text,a:Account){};

func test() : async (Account,Account){
     let reserve = await b.getReserve();
     let a1 = await (reserve.split(10));
     let a2 = await (reserve.split(10));
     show("reserve",reserve);
     show("a1",a1);
     show("a2",a2);
     await transfer(a1,a2,5);
     show("reserve",reserve);
     show("a1",a1);
     show("a2",a2);
     (a1,a2);
};
    
let main = test();
/*
let main = async ({let reserve = await b.getReserve();
                   let a1 = await (reserve.split(10));
		   let a2 = await (reserve.split(10));
   		    await transfer(a1,a2,5);
		   (a1,a2);} : (Account,Account)) ;
*/		  
