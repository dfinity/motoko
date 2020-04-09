Reference manual generation
===========================

This directory contains the tools to create the reference manual.

## Building

Run `make`. The output is in `_out`.

## Anchors

The tool creates HTML anchors of these forms:

 * `#mod-Foo` for the module `Foo`
   (These are actually not generated, but are put in manually)
 * `#Foo_bar` for value `bar` in module `Foo`
 * `#Foo_Bar` for type `Bar` in module `Foo`

To refer to them, use asciidoc syntax, as in

    Also see <<List_map,the `map` function for lists>>.

## Conventions and assumptions

See `stdlib/src/List.mo` for inspiration.

The doc generation tools requires the standard library code to be in a certain
shape:

 * No nested modules.

 * All public definitions are of the form
   `public let val : typ =` (in one line), or
   `public type name = …` (may span multiple lines, must be followed by a blank line).

 * Documentation comments have the form

       /**
       comment
       */

   The common indentation is removed. The body should be indented no further
   than the opening `/` (unless you want the indentation to carry over to the
   ascii document, e.g. for inline code blocks).

 * A documentation comment must be followed by either

   - a blank line
     then it is copied verbatim
   - a public value or type definition,
     then it is put between the header and the type listing for that definition

## Turning function declarations into let declarations

To appease this rigid regine, one has to rewrite

    public func foo(a : Bool) : Text = {
      body
    };

into

    public let foo : Bool -> Text = func(a) {
      body
    };

If the function is polymorphic, one also has to repeat the types, turning

    public func foo<T>(a : T, f : T -> T) : T = {
      body
    };

into

    public let foo : <T> (T, f : T -> T) -> T = {
      func<T>(a : T, f : T -> T) : T = {
        body
      };

Note that one can still _optionally_ name parameters. By convention, this is
only necessary when the name indicates semantic meaning (e.g. `eq : (T,T) ->
Bool`), or if it is referred to by the doc text (“the function `f`”).


