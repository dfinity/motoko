import List "list.mo";
import Option "option.mo";

module {

  type List<T> = List.List<T>;

  public func append(x : Text, y : Text) : Text {
    x # y;
  };

  public func toList(text : Text) : List<Char> {
    let get = text.chars().next;
    List.tabulate<Char>(text.len(), func _ {
      Option.unwrap<Char>(get())
    })
  };

}
