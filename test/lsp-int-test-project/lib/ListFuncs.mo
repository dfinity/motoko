import List = "./ListLib";

module {
  public func singleton<T>(x: T): List.List<T> =
    List.cons<T>(x, List.nil<T>());

  public func doubleton<T>(x: T, y: T): List.List<T> =
   List.cons<T>(x, List.cons<T>(x, List.nil<T>()));
}
