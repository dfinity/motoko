let A = (import "../serverActor.as");

actor test {

  func go() : async () {
      let s = A.Server();

      let r = await s.registrarAddRegion("region", "");
      print "\nAdded region";

      ignore(await s.registrarAddUser("0", "0", "", 0, true, true, true, true)); print "\nAdded user 0";
      ignore(await s.registrarAddUser("1", "1", "", 0, true, true, true, true)); print "\nAdded user 1";
      ignore(await s.registrarAddUser("2", "2", "", 0, true, true, true, true)); print "\nAdded user 2";
      ignore(await s.registrarAddUser("3", "3", "", 0, true, true, true, true)); print "\nAdded user 3";
      ignore(await s.registrarAddUser("4", "4", "", 0, true, true, true, true)); print "\nAdded user 4";
      ignore(await s.registrarAddUser("5", "5", "", 0, true, true, true, true)); print "\nAdded user 5";
      ignore(await s.registrarAddUser("6", "6", "", 0, true, true, true, true)); print "\nAdded user 6";
      ignore(await s.registrarAddUser("7", "7", "", 0, true, true, true, true)); print "\nAdded user 7";
      ignore(await s.registrarAddUser("8", "8", "", 0, true, true, true, true)); print "\nAdded user 8";
      ignore(await s.registrarAddUser("9", "9", "", 0, true, true, true, true)); print "\nAdded user 9";
      ignore(await s.registrarAddUser("10", "10", "", 0, true, true, true, true)); print "\nAdded user 10";
      ignore(await s.registrarAddUser("11", "11", "", 0, true, true, true, true)); print "\nAdded user 11";
      ignore(await s.registrarAddUser("12", "12", "", 0, true, true, true, true)); print "\nAdded user 12";
      ignore(await s.registrarAddUser("13", "13", "", 0, true, true, true, true)); print "\nAdded user 13";
      ignore(await s.registrarAddUser("14", "14", "", 0, true, true, true, true)); print "\nAdded user 14";
      ignore(await s.registrarAddUser("15", "15", "", 0, true, true, true, true)); print "\nAdded user 15";
      ignore(await s.registrarAddUser("16", "16", "", 0, true, true, true, true)); print "\nAdded user 16";
      ignore(await s.registrarAddUser("17", "17", "", 0, true, true, true, true)); print "\nAdded user 17";
      ignore(await s.registrarAddUser("18", "18", "", 0, true, true, true, true)); print "\nAdded user 18";
      ignore(await s.registrarAddUser("19", "19", "", 0, true, true, true, true)); print "\nAdded user 19";
      ignore(await s.registrarAddUser("20", "20", "", 0, true, true, true, true)); print "\nAdded user 20";
      ignore(await s.registrarAddUser("21", "21", "", 0, true, true, true, true)); print "\nAdded user 21";
      ignore(await s.registrarAddUser("22", "22", "", 0, true, true, true, true)); print "\nAdded user 22";
      ignore(await s.registrarAddUser("23", "23", "", 0, true, true, true, true)); print "\nAdded user 23";
      ignore(await s.registrarAddUser("24", "24", "", 0, true, true, true, true)); print "\nAdded user 24";
      ignore(await s.registrarAddUser("25", "25", "", 0, true, true, true, true)); print "\nAdded user 25";
      ignore(await s.registrarAddUser("26", "26", "", 0, true, true, true, true)); print "\nAdded user 26";
      ignore(await s.registrarAddUser("27", "27", "", 0, true, true, true, true)); print "\nAdded user 27";
      ignore(await s.registrarAddUser("28", "28", "", 0, true, true, true, true)); print "\nAdded user 28";
      ignore(await s.registrarAddUser("29", "29", "", 0, true, true, true, true)); print "\nAdded user 29";
  };
};
