// tests that object fields are properly sorted after inference/checking
func bad(.{ name : Text; age : Nat }) : Text = "text";
func ok(.{ age : Nat; name : Text }) : Text = "text";
{ let .{name; age} = .{name = "fred"; age = 40};};
{ let .{age; name} = .{name = "fred"; age = 40};};
