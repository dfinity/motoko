# The function call
#
#   gitSource ./toplevel subpath
#
# creates a Nix store path of ./toplevel/subpath that includes only those files
# tracked by git. More precisely: mentioned in the git index (i.e. git add is enough
# to get them to be included, you do not have to commit).
#
# This is a whitelist-based alternative to manually listing files or using
# nix-gitignore.

# Internally, it works by calling git ls-files at evaluation time. To
# avoid copying all of `.git` to the git store, it only copies the least amount
# of files necessary for `git ls-files` to work; this is a bit fragile, but
# very fast.

with builtins;

# We read the git index once, before getting the subdir parameter, so that it
# is shared among multiple invocations of gitSource:

let
  filter_from_list = root: files:
    let
      all_paren_dirs = p:
        if p == "." || p == "/"
        then []
        else [ p ] ++ all_paren_dirs (dirOf p);

      whitelist_set = listToAttrs (
        concatMap (p:
          let full_path = toString (root + "/${p}"); in
          map (p': { name = p'; value = true; }) (all_paren_dirs full_path)
        ) files
      );
    in
    p: t: hasAttr (toString p) whitelist_set;

  has_git_dir = builtins.pathExists (toString ./.. + "/.git/index");

  nixpkgs = (import ./nixpkgs.nix).nixpkgs {};

  pruned_git_dir = path {
    name = "git-dir";
    path = ../.git;
    filter = filter_from_list ../.git
      ["index" "refs" "objects" "HEAD" "config"];
  };

  whitelist_file =
    nixpkgs.runCommand "git-ls-files" {envVariable = true;} ''
      ${nixpkgs.git}/bin/git --git-dir ${pruned_git_dir} ls-files > $out
    '';

  lines = s: filter (x : x != [] && x != "") (split "\n" s);

  whitelist = lines (readFile (whitelist_file.out));

  in_whitelist = filter_from_list ../. whitelist;
in

subdir: path {
  name = baseNameOf (toString subdir);
  path = if isString subdir then (../. + "/${subdir}") else subdir;
  filter = if has_git_dir then in_whitelist else (p: t: true);
}
