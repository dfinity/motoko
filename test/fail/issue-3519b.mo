// reject generic shared functions
do { type wrong1 = shared <T>()-> async (); };
do { type wrong2 = shared query <T>()-> async (); };
do { type wrong3 = shared <T>()->(); };
