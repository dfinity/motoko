shared (deployer) actor class Test() {
  public shared (context) func test(): async () {
    await async {
      loop {};
    };
  };
};
