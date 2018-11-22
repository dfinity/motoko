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
    balance := 0;
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


 
// Example usage
func transfer(sender : Account, receiver : Account, amount : Int) : async ()  {
  let trx = await sender.split(amount);
  receiver.join(trx);
};

