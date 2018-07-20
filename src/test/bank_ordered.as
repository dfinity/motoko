// Like bank.as but in dependency order

actor class Account(initial_balance : Int) {
  private var balance : Int = initial_balance;

  getBalance() : async Int {
    return balance;
  };

  split(amount : Int) : async Account {
    balance -= amount;
    return Account(amount);
  };

  join(account : Account) {  // this implicitly asserts that account is Account
    let amount = balance;
    balance := +0;  // Hack!
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

actor class Issuer() {
  hasIssued(account : like Account) : async Bool {
    return (account is Account);
  };
};

actor class Bank(supply : Int) {
  private issuer = Issuer();
  private reserve = Account(supply);
  getIssuer() : async Issuer { return issuer; };
  getReserve() : async Account { return reserve ; };
};

 
// Example usage
func transfer(sender : Account, receiver : Account, amount : Int) : async ()  {
  let trx = await sender.split(amount);
  trx.join(receiver);
};

