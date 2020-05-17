# LexerInLisp

This project implements a lexer that does the tokenization of any set of valid G++ expressions or statements.

G++ is a kind of programming language that is not real.

It takes a file or an input from console and do lexical analysis.


# Rules of G++ Language

The rules are given in the /Files/G++Syntax.pdf


# How to run

1. Download Ubuntu clisp package

2. Load gpp_lexer.lisp file

3. Call gppinterpreter OR gppinterpreter "filename" 


# Example

Input:

<pre><code>
(deffun sumup (x)
  (if (equal x 0)
    1
    (+ x (sumup (- x 1))) 
  )   
)
</code></pre>


Output:
<pre><code>
COMMENT
OP_OP
KW_DEFFUN
IDENTIFIER
OP_OP
IDENTIFIER
OP_CP
OP_OP
….
</code></pre>

