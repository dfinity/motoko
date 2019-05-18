class OK<T>() {
  type T = Int;
  type U = T
};

class Wrong1<T>(){ type U = T; };

module class Wrong2<T>(){ type U = T; };