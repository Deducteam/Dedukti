[![Build Status](https://travis-ci.org/Deducteam/Dedukti.svg?branch=develop)](https://travis-ci.org/Deducteam/Dedukti.svg?branch=develop)

See the Dedukti's webpage : http://dedukti.gforge.inria.fr/

1) User
You can install Dedukti easely by typing the following commands:
- make configure
- make build
- make install

2) Developer

2.1) Oasis
The compilation of Dedukti uses oasis. You can install oasis with opam by typing:
- opam install oasis

Most of the time, you won't change the project. Therefore, you only need to use the commande `make build`.
However, if you add new modules, or tools, you have to modify the _oasis file.
To update the Makefile and some other files, you have to run the command `oasis setup` to take care of the
modifications made in the _oasis file.

2.2) Opam
You can setup Opam to install the development version of Dedukti
instead of the last stable version by typing the following commands in
the current directory:
- opam pin -k git add dedukti .
- opam upgrade
