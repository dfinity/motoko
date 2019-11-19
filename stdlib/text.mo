import List "list.mo";
import Option "option.mo";

type List<T> = List.List<T>;

module {

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
