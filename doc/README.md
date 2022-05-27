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

## Page attributes

The interpreter is controlled by the following page attributes:
 * `page-repl` controls if the page needs to load the interpreter (1.2M)
 * `page-moc-version` specifies the moc version
 * `page-moc-base-tag` specifies the base library version

`page-moc-version` and `page-moc-base-tag` are set globally in [antora-playbook.yml](https://github.com/dfinity/dfinity-docs-playbook/blob/master/antora-playbook.yml), and `page-repl` is set in `antora.yml` in Motoko component, which means
the interpreter is enabled for all pages in Motoko docs by default.
If you want to disable the interpreter for a particular page, use `:!page-repl:` at the top of the page
(syntax highlighting still works).

After each release, remember to bump the version in [antora-playbook.yml](https://github.com/dfinity/dfinity-docs-playbook/blob/master/antora-playbook.yml).

## Source flags

In asciidoc, we support the following language sources:

* `[source, motoko]` adds a run button
* `[source.run, motoko]` adds a run button and displays the result.
* `[source.no-repl, motoko]` syntax-highlight only. If you want to set `no-repl` for the whole page, you can disable the page attributes `:!page-repl:`, and use just `[source, motoko]`.
* `[source#filename, motoko]` save the code as `filename.mo` so that it can be imported or referenced from another code block. An absent filename defaults to `stdin`.
* `[source.include_f1_f2, motoko]` run `f1.mo`, `f2.mo` before running the current code, equivalent to `moc -i f1.mo f2.mo current_code.mo`. It will fetch the updated code each time we click run. Note that if the code `import "f1"` but doesn't use `include_f1`, the code won't be updated until we click the run button for `f1.mo`.
* `[source, candid]` syntax-highlights a Candid block or file without a run button.

The control flags can be used in any order with any combinations, e.g. `[source.run#main.include_f1_f2, motoko]`.

## Customization

If you have more advanced needs beyond what is provided, you can hook a JS function to the `Run` button.
For example, the following code wraps the `include` code in an actor before running,
and format the output.

```
++++
<script>
function myRun(includes, current_code) {
  const code = `
    actor {
      ${includes["f1.mo"]}
      ${includes["f2.mo"]}
    };
    ${current_code}
  `;
  Motoko.saveFile("myMain.mo", code);
  Motoko.saveFile("type.mo", 'type Num = Nat;');
  const res = Motoko.run(["type.mo"], "myMain.mo");
  if (res.stderr) {
    res.stderr = "ERROR: " + res.stderr;
  }
  if (res.stdout) {
    res.stdout = "RESULT: " + res.stdout;
  }
  return res;
}
</script>
++++
[source.include_f1_f2.hook_myRun, motoko]
----
code here
----
```
