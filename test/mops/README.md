# Running moc type checker over sources in mops

This is very rought but might be useful again.

1. Edit packages.txt with mops packages to check

2. Run ./initmops.sh

   This downloads all the packages in packages.txt into .mops

3. Run ./verify.sh

   This creates a dummy import file _out/pkg@version.mo for each
   non-`base` package, importing all /src/* files for the package.

   It then compiles the generated file importing all non-base packages,
   using the base sources in $MOTOKO_BASE.

   Output is redirected to  _out/pkg@version.tc




