# Building the documentation locally

You can build the basic documentation locally as follows:

```
make
python3 -m http.server --directory html
# now open http://0.0.0.0:8000/motoko.html
```

CI pushes these docs for latest master to
<https://hydra.dfinity.systems/job/dfinity-ci-build/motoko/docs/latest/download/1/overview-slides.html>.

The local documentation is suboptimal and compiled with pandoc, not
docusaurus so it doesn't understand or process remark-code-import file includes,
docusaurus admonitions (`:: Tip` etc) nor enable the motoko interpreter.

For a richer preview that supports these features,
and auto-updates as you edit doc source, try:

```
make preview
```

This uses a small ./docusaurus project to build a reduced web site locally, opening it your browser. It should be a subset of the full portal documentation,
complete with live code blocks.

In order to preview the real portal documentation, open a PR
https://github.com/dfinity/portal, edit the git "submodule"for
Motoko to point at the PR branch and wait for CI to produce a build or
follow the instructions to produce one locally.

# Support the moc interpreter in documentation

We wrap Docusaurus's module `CodeBlock/Content/String` to process Motoko code blocks with interpreter.
To enable this feature, add the following flags for code blocks:

"```motoko" adds a Run button
"```motoko run" adds a Run button and display the result
"```motoko no-repl" syntax-highlighting only.
"```motoko name=filename" saves the code as filename.mo so that it can be imported or referenced from another code block. An absent filename defaults to stdin.
"```motoko include=f1,f2" run `f1.mo`, `f2.mo` before running the current code, equivalent to `moc -i f1.mo f2.mo current_code.mo`. It will fetch the updated code each time we click run. Note that if the code import "f1" but doesn't use include_f1, the code won't be updated until we click the run button for f1.mo.

The config flags can be used in any order with any combinations, e.g.
"```motoko run name=main include=f1,f2"
