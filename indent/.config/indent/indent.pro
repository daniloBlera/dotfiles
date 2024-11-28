// An implementation of the One True Brace style
//
// Some of the key features:
//  * procedure return type and name on the same line;
//  * opening braces stay in the same line;
//  * `while` and `else` cuddle braces;
//  * indent everything with four spaces;
//  * maximum line width of 90 columns;

-bad    // blank line after declarations
-bap    // blank line after procedure body
-bbb    // blank line before block comments
-br     // keep braces on `if` line
-brf    // keep braces on function definition line
-brs    // keep braces on struct declaration line
-cdw    // keep `while` on the same line as the closing `do` brace (in a `do-while`)
-ce     // keep `else` on the same line as its preceeding closing brace
-ci4    // indent continuations with 4 spaces
-cli4   // indent `case` labels with 4 spaces
-i4     // indent levels with 4 spaces
-il4    // indent labels with 4 spaces
-l90    // set max line length to 90 columns
-nlps   // remove space between '#' and preprocessor directive
-npcs   // remove space between function names and their parenthesis
-npsl   // keep a procedure's return type and and its name on the same line
-nut    // replace tab indents with spaces
-sob    // swallow optional blank lines
-ts4    // set tab size to 4 spaces
