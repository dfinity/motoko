// Importing `lib/Array` so it gets checked before this module. Hopefully in a real project
// there will have been a reference to this somewhere upstream
import {} "lib/Array"

func _suggest_import() {
  [1, 2, 3].first()
};

func _no_matching_module_in_scope() {
  [1, 2, 3].last()
};
