actor class Bank(supply : Int) {
  private issuer = Issuer();
  private reserve = Account(supply);
  getIssuer() : async Issuer { return issuer; };
  getReserve() : async Account { return reserve; };
};

actor class Issuer() {
  hasIssued(account : like Account) : async Bool {
    return (account is Account);
  };
};

actor class Account(initialBalance : Int) = self {
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
    balance := 0;
    account.credit(amount, Account);
  };

  credit(amount : Int, caller : Class) {
    assert(self is caller);
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
