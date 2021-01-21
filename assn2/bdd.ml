open Formula;;
module BDD = struct
		type sat_assignment = string list
		
		(* define the type robdd in whatever way you want  *)
    type robdd = Node_root of bool
              | Node_node of string * robdd * robdd ;;
		        
		exception Not_implemented
    exception Not_possible
    exception Wrong_bool_expr
    exception NoSatSolForContradiction
    exception NoStringAtLeaf
    exception NoChildAtLeaf
    exception NoBoolAtIntermidiateNode

    let number_of_entries = 1000000;;
    (*mapping (depth, left_child_entry, right_child_entry) -> parent_entry *)
    let child_parent = Hashtbl.create number_of_entries;;
    (*mapping parent_entry -> (depth, left_child_entry, right_child_entry) *)
    let parent_child = Hashtbl.create number_of_entries;;
    (*every table entry corresponds unique node*)
    let entry_node = Hashtbl.create number_of_entries;;
    (*every unique node corresponds table entry*)
    let node_entry = Hashtbl.create number_of_entries;;
    (*mapping string to bool*)
    let truth_assn = Hashtbl.create 100;;

    (* !!! REMEMBER !!!*)
    (* = for deep value check *)
    (* == for address check*)

    let stringEquiv n1 = match n1 with Node_node(str, _, _) ->  str
                                      | _  ->  raise Not_possible;;
    let stringFromEntry e =
      let n1 = Hashtbl.find entry_node e in
      stringEquiv n1
    ;;
    
    (* update the various hash tables *)
    let makeEntry string_at_i i l h= 
      if l == h then l
      else if Hashtbl.mem child_parent (i, l, h) then
        Hashtbl.find child_parent (i, l, h)
      else (
        let u = Hashtbl.length parent_child in 
          Hashtbl.add parent_child u (i, l, h);
          Hashtbl.add child_parent (i, l, h) u;
          let left_node = Hashtbl.find entry_node l in
          let right_node = Hashtbl.find entry_node h in
          Hashtbl.add entry_node u (Node_node(string_at_i, left_node, right_node));
          Hashtbl.add node_entry (Hashtbl.find entry_node u) u;
          u
      )
    ;;
    
    (* read the truth assn of each string from hash table
     * and return truth value of bexpr
     *)
    let rec eval bexpr = 
      match bexpr with
      | Program.OprUnary(Program.NOT, expr)                         ->  not (eval expr)
      | Program.OprBinary(Program.AND, expr1, expr2)                ->  (eval expr1) && (eval expr2)
      | Program.OprBinary(Program.OR, expr1, expr2)                 ->  (eval expr1) || (eval expr2)
      | Program.OprBinary(Program.IFTHEN, expr1, expr2)             ->  (not (eval expr1)) || (eval expr2)
      | Program.OprBinary(Program.IFF, expr1, expr2)                ->  let p = eval expr1 in 
                                                                        let q = eval expr2 in
                                                                        (p && q) || ((not p) && (not q))
      | Program.OprTernary(Program.IFTHENELSE, expr1, expr2, expr3) ->  let p = eval expr1 in
                                                                        if p then eval expr2
                                                                        else eval expr3
      | Program.Variable(str)                                       ->  Hashtbl.find truth_assn str
      | Program.Constant(b)                                         ->  b
      | _                                                           ->  raise Wrong_bool_expr
    ;;

    (*init parent_child hash table with first two rows*)
    let bddFromExpr bexpr order =
      (*clearing the old hash table values*)
      Hashtbl.clear parent_child;
      Hashtbl.clear child_parent;
      Hashtbl.clear truth_assn;
      Hashtbl.clear entry_node;
      Hashtbl.clear node_entry;

      let total_vars = List.length order
      in (
        (* Function build() *)
        let rec build bexpr order depth =
          (* base caes *)
          if depth > total_vars then (
            (if eval bexpr then 1 else 0)
          )
          else (
            match order with
            |[]     ->  raise Not_possible
            |h::t   ->  (
                          Hashtbl.add truth_assn h false;
                          let v0 = build bexpr t (depth+1) in
                          Hashtbl.replace truth_assn h true;
                          let v1 = build bexpr t (depth+1) in
                          Hashtbl.remove truth_assn h;
                          makeEntry h depth v0 v1
                        )
            )
        in (
          Hashtbl.add parent_child 0 (total_vars+1, -1, -1);
          Hashtbl.add parent_child 1 (total_vars+1, -1, -1);
          Hashtbl.add entry_node 0 (Node_root(false));
          Hashtbl.add entry_node 1 (Node_root(true));
          Hashtbl.add node_entry (Hashtbl.find entry_node 0) 0;
          Hashtbl.add node_entry (Hashtbl.find entry_node 1) 1;

          let root_index = build bexpr order 1 in
          Hashtbl.find entry_node root_index
        )
      )
    ;;

    (*fast power function*)
    (*!! OVERFLOW STARTS AT power 2 62 !!*)
    let rec power a n =
      if n == 0 then 1
      else if n == 1 then a
      else if (n mod 2 == 0) then
        let prod = power a (n/2) in prod * prod
      else
        let prod = power a ((n-1)/2) in
        a * prod * prod
    ;;

    (* Count number of satisfing truth assignments *)
    let sat_count n1 =
      let rec count entry =
        if entry = 0 then 0
        else if entry = 1 then 1
        else (
          let (depth, low_entry, high_entry) = Hashtbl.find parent_child entry in
          let (low_entry_depth, _, _) = Hashtbl.find parent_child low_entry in
          let (high_entry_depth, _, _) = Hashtbl.find parent_child high_entry in
          (count(low_entry) * (power 2 (low_entry_depth - depth - 1))) +
          (count(high_entry) * (power 2 (high_entry_depth - depth - 1)))
        )
      in
        let root_entry = Hashtbl.find node_entry n1 in
        let (root_depth, _, _) = Hashtbl.find parent_child root_entry in
        (power 2 (root_depth-1)) * count(root_entry)
    ;;
    
    (* Find one solution if possible *)
    let any_sat n1 =
      let rec anySat entry =
        let (depth, low_entry, high_entry) = Hashtbl.find parent_child entry in
        if entry == 0 then raise NoSatSolForContradiction
        else if entry == 1 then []
        else if low_entry == 0 then
          let var_str = stringFromEntry entry in
          var_str::anySat high_entry
        else anySat low_entry
      in
        let root_entry = Hashtbl.find node_entry n1 in
        anySat root_entry
    ;;

    (* append two lists *)
    let rec append l1 l2 =
      match l1 with
      | []    ->  l2
      | h::t  ->  h :: append t l2
    ;;

    (*append an element to all sub lists in a list*)
    let rec appendToAll e l=
      match l with
      | []    ->  []
      | h::t  ->  append [e::h] (appendToAll e t)
    ;;

    let all_sat n1 =
      let rec allSat entry =
        if entry == 0 then []
        else if entry == 1 then [[]]
        else (
          let (depth, low_entry, high_entry) = Hashtbl.find parent_child entry in
          let low_sols = allSat low_entry in
          let high_sols = allSat high_entry in
          let curr_str = stringFromEntry entry in
          append low_sols (appendToAll curr_str high_sols)
        )
      in
        let root_entry = Hashtbl.find node_entry n1 in
        allSat root_entry
    ;;
		
    (*write input bdd in "bdd.dot" file in format accepted by DOT*)
		let to_dot bdd =
      (*keep trach of visited node to avoid duplicat edges*)
      let visited = Hashtbl.create number_of_entries in
      let printEdge root_str root_entry n tail_label color file = 
        let curr_entry = Hashtbl.find node_entry n in
        match n with
        | Node_root(true)   ->  Printf.fprintf file "%s_%d -- 1 [taillabel = %s, color = %s]\n" root_str root_entry tail_label color
        | Node_root(false)  ->  Printf.fprintf file "%s_%d -- 0 [taillabel = %s, color = %s]\n" root_str root_entry tail_label color
        | Node_node(s, _, _)->  Printf.fprintf file "%s_%d -- %s_%d [taillabel = %s, color = %s]\n" root_str root_entry s curr_entry tail_label color
      in
      let rec _to_dot bdd file =
        let entry = Hashtbl.find node_entry bdd in
        if Hashtbl.mem visited bdd then ()
        else (
          Hashtbl.add visited bdd true;
          match bdd with
          | Node_root(true)     ->  Printf.fprintf file "1 [color = green]\n";
                                    Printf.fprintf file "1 [xlabel = %d, label = 1]\n" entry
          | Node_root(false)    ->  Printf.fprintf file "0 [color = red]\n";
                                    Printf.fprintf file "0 [xlabel = %d, label = 0]\n" entry
          | Node_node(s, l, r)  ->  Printf.fprintf file "%s_%d [xlabel = %d, label = %s]\n" s entry entry s;
                                    (printEdge s entry l "0" "red" file) ;
                                    (printEdge s entry r "1" "green" file) ;
                                    (_to_dot l file) ;
                                    (_to_dot r file)
        )
      in
      let file = open_out "bdd.dot" in
      Printf.fprintf file "graph {\n";
      _to_dot bdd file;
      Printf.fprintf file "}";
      close_out file
    ;;
		
end  ;;
(* let a = (Program.Constant true);;
a;; *)
