(** Size change principle.
    This module implements a decision procedure based on the work of
Chin Soon Lee, Neil D. Jones and Amir M. Ben-Amram (POPL 2001).
    Most of this module comes from an implementation of Rodolphe Lepigre
and Christophe Raffalli. *)
open Basic
open Term
open Rule
open Sizematrix
open Callgraph

(** the main function, checking if calls are well-founded *)
let sct_only : unit -> unit =
  fun ()->
    let ftbl= !graph in
    let num_fun = !(ftbl.next_index) in
    (* tbl is a num_fun x num_fun Array in which each element is the list of all matrices between the two symbols with the rules which generated this matrix *)
    let tbl = Array.init num_fun (fun _ -> Array.make num_fun []) in
    let print_call ff= pp_call ff in 
  (* counters to count added and composed edges *)
    let added = ref 0 and composed = ref 0 in
  (* function adding an edge, return a boolean indicating
     if the edge is new or not *)
    let add_edge i j m r =
      let ti = tbl.(i) in
      let ms = ti.(j) in
      if List.exists (fun m' -> subsumes (fst m') m) ms
      then
	false
      else
        begin
          (* test idempotent edges as soon as they are discovered *)
          if i = j && prod m m = m && not (decreasing m) then
            begin
	    Debug.(debug d_sizechange "edge %a idempotent and looping\n%!" print_call
                  {callee = i; caller = j; matrix = m; rules = r});
              update_result i (SelfLooping r)
	    end;
	  let ms = (m, r) ::
            List.filter (fun m' -> not (subsumes m (fst m'))) ms in
          ti.(j) <- ms;
          true
        end
    in
    (* adding initial edges *)
    Debug.(debug d_sizechange "initial edges to be added:");
    List.iter
      (fun c -> Debug.(debug d_sizechange "  %a" print_call c))
      !(ftbl.calls);
    let new_edges = ref !(ftbl.calls) in
    (* compute the transitive closure of the call graph *)
    Debug.(debug d_sizechange "start completion");
    let rec fn () =
      match !new_edges with
      | [] -> ()
      | ({callee = i; caller = j; matrix = m; rules=r} as c)::l ->
        new_edges := l;
        if add_edge i j m r
        then
          begin
            Debug.(debug d_sizechange "  edge %a added" print_call c);
            incr added;
            let t' = tbl.(j) in
            Array.iteri
              (fun k t ->
                 List.iter
                   (fun (m',r') ->
                      let c' = {callee = j; caller = k; matrix = m'; rules=r'}
                      in
                      Debug.(debug d_sizechange "  compose: %a * %a = "
                          print_call c
                          print_call c');
                      let m'' = prod m m' in
                      let r'' = r @ r' in
                      incr composed;
                      let c'' =
                        {callee = i; caller = k; matrix = m''; rules = r''}
                      in
                      new_edges := c'' :: !new_edges;
                       Debug.(debug d_sizechange "%a" print_call c'');
                   ) t
              ) t'
        end else
        Debug.(debug d_sizechange "  edge %a is old" print_call c);
        fn ()
    in
    fn ();
     Debug.(debug d_sizechange "SCT passed (%5d edges added, %6d composed)"
        !added !composed)


