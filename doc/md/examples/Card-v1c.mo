import CardMigration "CardMigration";

persistent actor
  [ CardMigration.migrate ] // Declare the migration function
  {
  type Card = {
    title : Text;
    description : Text;
  };

  var map : [(Nat32, Card)] = []; // Initialized by migration on upgrade

};
