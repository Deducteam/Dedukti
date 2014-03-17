DEDUKTI v2.2c
============
USER MANUAL
-----------

### INSTALLATION


In order to compile Dedukti you need OCaml (>= 3.12) and Menhir.

#### FROM OPAM

    opam install dedukti

#### FROM SOURCES

Download Dedukti at https://gforge.inria.fr/frs/download.php/33466/dedukti-v2.2c.tar.gz.

    tar xfzv dedukti-v2.2c.tar.gz
    cd dedukti-v2.2c
    make
    sudo make install

### PROGRAMS

#### DKCHECK

`dkcheck` is the type-checker for Dedukti.

##### QUICK START

    $ dkcheck examples/append.dk
    > File examples/append.dk was successfully Checked.

##### A SMALL EXAMPLE

A Dedukti file must begin with the name of the module.

    #NAME my_module.

Then we can declare some constants:

    Nat: Type.
    Zero: Nat.
    Succ: Nat.
    Plus: Nat -> Nat -> Nat.

We can now add rewrite rules:

[n:Nat] Plus Zero n --> n.
[n:Nat:m:Nat] Plus (Succ n) m --> Succ (Plus n m).

It is also possible write definitions:

    Three := Succ ( Succ ( Succ ( Zero ) ) ).

##### SEPARATED COMPILATION

A development in Dedukti is usually composed of several files corresponding to different modules.
In order You can export the symbols declared in a module by checking the file with the option `-e`.
Then you can use these symbols using the prefix notation `module_name.identifier`.

#### DKTOP & DKDEP
Dedukti comes with two additional tools:

  * `dktop` is an interactive wrapper around the typechecker.
  * `dkdep` is a dependency generator for `.dk` files.

### GRAMMAR

    program     : prelude line*

    prelude     : '#NAME' ident '.'

    line        : ident ':' term '.'                            /* Declaration                                */
                | ident ':=' term '.'                           /* Definition                                 */
                | ident ':' term ':=' term '.'                  /* Definition with type                       */
                | ident param+ ':=' term '.'                    /* Definition with parameters                 */
                | ident param+ ':' term ':=' term '.'           /* Definition with parameters and type        */
                | '{' ident '}' ':' term ':=' term '.'          /* Opaque definition with type                */
                | '{' ident '}' ':=' term '.'                   /* Opaque definition                          */
                | '{' ident param+ '}' ':=' term '.'            /* Opaque definition with parameters          */
                | '{' ident param+ '}' ':' term ':=' term '.'   /* Opaque definition with parameters and type */
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
                | ident ':' app '->' term   /* Dependent-type */
                | term '->' term            /* Non-dependent type */
                | ident ':' app '=>' term   /* Lambda abstraction */

### LICENCE

Dedukti is distributed under the CeCILL-B License.

