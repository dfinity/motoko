// This mainly exercises the multi-value-faking code in the backend

func returnsLargeTuple() : ((),(),(),(),(),(),(),(),(),(),(),(),(),(),(),()) = ((),(),(),(),(),(),(),(),(),(),(),(),(),(),(),());
func wantsLargeTuple((),(),(),(),(),(),(),(),(),(),(),(),(),(),(),()) = ();
wantsLargeTuple(returnsLargeTuple());
