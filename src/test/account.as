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
    balance := +0;
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
func transfer(sender : Account, receiver : Account, amount : Int) : async ()  {
  let trx = await sender.split(amount);
  receiver.join(trx);
};

