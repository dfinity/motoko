// tests that object fields are properly sorted after inference/checking
func bad({ name : Text; age : Nat }) : Text = "text";
func ok({ age : Nat; name : Text }) : Text = "text";
do { let {name; age} = {name = "fred"; age = 40};};
do { let {age; name} = {name = "fred"; age = 40};};
