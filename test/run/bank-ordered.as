// Like bank.as but in dependency order

actor class Account(initialBalance : Int) = this {
  private var balance : Int = initialBalance;

  getBalance() : async Int {
    return balance;
  };

  split(amount : Int) : async Account {
    balance -= amount;
    return Account(amount);
  };

  join(account : like Account) {
    assert(account is Account);
    let amount = balance;
    balance := +0;  // Hack!
    account.credit(amount, Account);
  };

  credit(amount : Int, caller : Class) {
    assert(this is caller);
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

