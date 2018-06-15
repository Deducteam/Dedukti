open Basic
open Term
open Rule
open Format
    
(** Representation of the set {-1, 0, ∞} *)
type cmp = Min1 | Zero | Infi

(** String representation. *)
val cmp_to_string : cmp -> string
  
(** The pretty printer for the type [cmp] *)
val pp_cmp : cmp printer

(** Addition operation (minimum) *)
val (<+>) : cmp -> cmp -> cmp

(** Multiplication operation. *)
val (<*>) : cmp -> cmp -> cmp


(** Type of a size change matrix. *)
type matrix =
  { w   : int             ; (* Number of argument of callee *)
    h   : int             ; (* Number of argument of caller *)
    tab : cmp array array   (* The matrix of size h*w *)
  }

(** The pretty printer for the type [matrix] *)
val pp_matrix : matrix printer
    
(** Matrix product. *)
val prod : matrix -> matrix -> matrix
      
(** Check if a matrix corresponds to a decreasing idempotent call. *)
val decreasing : matrix -> bool

(** Check if a matrix subsumes another one (i.e. gives more infomation). *)
val subsumes : matrix -> matrix -> bool

(** [matrix_of_lists m lp n lt n] compare each term of a list [lt] with a list of pattern [lp] considering that we are under [nb] lambdas and add some Infi to respect the arities of the caller and called functions *)
val matrix_of_lists : int -> pattern list -> int -> term list -> int -> matrix
