# Building the documentation locally

You can build the documentation locally as follows:

```
make
python3 -m http.server --directory build/site/
# now open http://0.0.0.0:8000/docs/language-guide/motoko.html
```

This is somewhat hackish

 * Uses `antora-test-playbook.yml` instead of the official playbook in
   `dfinity/dfinity-docs-playbook`
 * Uses a local copy `test-ui-bundle.zip` of the file from `dfinity/antora-sdk`
 * Uses the script `patch-antora-js.sh` to change some URLs in the JS of the
   generated files to load `moc.js` and the base library sources from the
   _current_ revision.

This will likely easily break when things change in the official playbook and
the official UI bundle.

CI pushes these docs for latest master to
<https://hydra.dfinity.systems/job/dfinity-ci-build/motoko/docs/latest/download/1/overview-slides.html>.


# Support interpreter in documentation

We wrap Docusaurus's module `CodeBlock/Content/String` to process Motoko code blocks with interpreter. 
To enable this feature, add the following flags for code blocks:

"```motoko" adds a Run button
"```motoko run" adds a Run button and display the result
"```motoko no-repl" syntax-highlighting only.
"```motoko name=filename" saves the code as filename.mo so that it can be imported or referenced from another code block. An absent filename defaults to stdin.
"```motoko include=f1,f2" run `f1.mo`, `f2.mo` before running the current code, equivalent to `moc -i f1.mo f2.mo current_code.mo`. It will fetch the updated code each time we click run. Note that if the code import "f1" but doesn't use include_f1, the code won't be updated until we click the run button for f1.mo.

The config flags can be used in any order with any combinations, e.g.
"```motoko run name=main include=f1,f2"

