module {
  // No way to opt-out of stable-ness here?
  public class Map<K, V>(compare : stable (K, K) -> Int) {
    public func insert(key : K, value : V) : ?V {
      ignore compare(key, key);
      null
    }
  }
}
