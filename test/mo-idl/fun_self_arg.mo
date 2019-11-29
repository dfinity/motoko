type F = shared () -> async F;
type A = actor { foo : F }
