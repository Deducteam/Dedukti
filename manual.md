DEDUKTI v2.3
============
USER MANUAL
-----------

### INSTALLATION

In order to compile Dedukti you need OCaml (>= 3.12) and Menhir.

#### OPAM

    opam install dedukti

#### FROM SOURCES

    git clone blabla dir
    cd dir
    make 
    make install

### PROGRAMS

#### DKCHECK

`dkcheck` is the typechecker for Dedukti.

##### QUICK START

    $ dkcheck examples/append.dk
    > File examples/append.dk was successfully Checked.

##### USAGE
    Usage: dkcheck [options] files
      -q Quiet
      -v Verbose
      -e Create a .dko
      -nc Disable colored output
      -stdin Use standart input
      -r Ignore redeclaration
      -display_db Display DeBruijn indices when printing terms
      -version Version
      -autodep Automatically handle dependencies (experimental)
      -help  Display this list of options
      --help  Display this list of options

##### SEPARATED COMPILATION

TODO

##### WILDCARDS AND STATIC DECLARATIONS

TODO

##### COMMAND

The following commands can be used in your files:

 * `#Whnf` term: Compute the weak head normal form of a term.
 * `#Hnf` term: Compute the head normal form of a term.
 * `#Snf` term: Compute the strong normal form of a term.
 * `#OneStep` term: Rewrite a term. 
 * `#Conv`  term, term: Check that two term are convertible.
 * `#Check` term, term: Check that a term has a specific type.
 * `#Infer` term: Infer the type of a term.
 * `#Print` string : Print a string.

#### DKTOP & DKDEP
Dedukti comes with two additional tools:

  * `dktop` is an interactive wrapper around the typechecker.
  * `dkdep` is a dependency generator for `.dk` files.

### GRAMMAR

    program     : prelude line*
    
    prelude     : '#NAME' ident '.'
    
    line        : ident ':' term '.'                            /* Declaration                                */
                | '{' ident '}' ':' term '.'                    /* Opaque declaration                         */
                | ident ':=' term '.'                           /* Definition                                 */
                | ident ':' term ':=' term '.'                  /* Definition with type                       */
                | ident param+ ':=' term '.'                    /* Definition with parametes                  */
                | ident param+ ':' term ':=' term '.'           /* Definition with parameters and type        */
                | '{' ident '}' ':' term ':=' term '.'          /* Opaque definition wih type                 */
                | '{' ident '}' ':=' term '.'                   /* Opaque definition                          */	
                | '{' ident '}' param+ ':=' term '.'            /* Opaque definition with parameters          */
                | '{' ident '}' param+ ':' term ':=' term '.'   /* Opaque definition with parameters and type */
                | rule+ '.'                                     /* Rewrite rules                              */
                | '#' ident term_lst '.'                        /* Command                                    */
    
    param       : '(' decl ')'

    term_lst    : term 
                | term ',' term_lst 
    
    rule        : '[' context ']' top_pattern '-->' term
    
    decl        : ident ':' term
    
    context     : /* empty */ 
                | decl ',' context
                | decl 
    
    top_pattern : ident pattern*
    
    pattern     : ident
                | ident '.' ident
                | '_'
                | '(' ident  pattern* ')'
                | '(' ident '.' ident pattern* ')'
    
    sterm       : ident '.' ident           /* Qualified identifier */
                | ident                     /* Identifier */
                | '(' term ')'
                | 'Type'                    /* Type */
    
    app         : sterm 
                | sterm app
    
    term        : app                       /* Application */
                | ident ':' app '->' term   /* Dependant type */
                | term '->' term            /* Non-dependant type */
                | ident ':' app '=>' term   /* Lambda abstraction */

### LICENCE

