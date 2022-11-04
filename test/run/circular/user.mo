import article "article";
module {
  type article = article.article;
  public type user = {
    name : Text;
    article : article;
  };
};
