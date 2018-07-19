open Term

type const = Kind1
	     | Type1 
	     | DB1    of int 
	     | Const1 of name
	     | E of int;;

type signature = App1 of const * const * const list
		 | Lam1 of const option * const
		 | Pi1 of const * const
		 | Clos1 of const * const list;;
