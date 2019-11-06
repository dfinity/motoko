type post = shared Text -> future ();

type IServer = actor {
  post: Text -> future ();
  subscribe: IClient -> future post;
};

type IClient = actor {
   send: shared Text -> future ();
};
