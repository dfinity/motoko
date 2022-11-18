import user "user";
module{
  type user = user.user;
  public type article = {
    name : Text;
    u : user;
  };
};
