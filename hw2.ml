(* Definitions *)
type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

type ('nonterminal, 'terminal) parse_tree = 
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal


(* 1. Write a function convert_grammar gram1 that returns a Homework 
2-style grammar, which is converted from the Homework 1-style grammar gram1. *)
let convert_grammar (init_symbol, gram1) =
  (* sort the rules by their LHS to facilitate grouping *)
  let rules = List.sort (fun (x, _) (y, _) -> compare x y) gram1 in

  (* symbol_helper processes a sorted list of grammar rules to accumulate all
   RHS for a specific LHS symbol (cur_symbol). It returns a tuple with a list 
   of RHS for that symbol and the rest of the unprocessed rules. *)
  let rec symbol_helper g acc cur_symbol = 
    match g with
    | [] -> (List.rev acc, [])
    | (lhs, rhs) :: t ->
        if lhs = cur_symbol then 
          symbol_helper t (rhs :: acc) cur_symbol
        else 
          (List.rev acc, g)
  in

  (* helper processes the entire list of grammar rules to group all RHS by
  their LHS. It uses symbol_helper to process each new LHS encountered. *)
  let rec helper g acc =
    match g with
    | [] -> List.rev acc
    | (lhs, rhs) :: t ->
        let (cur, rest) = symbol_helper t [rhs] lhs in
        let cur_acc = (lhs, cur) in
        helper rest (cur_acc :: acc)
  in
  let converted_rules = helper rules [] in 
  let rule_func symbol = 
    match List.assoc_opt symbol converted_rules with
    | Some rules -> rules
    | None -> []
  in
  (init_symbol, rule_func)


(* 2. write a function parse_tree_leaves tree that traverses the parse tree 
   left to right and yields a list of the leaves encountered, in order.*)
   let rec parse_tree_leaves tree =
    match tree with
    | Leaf l-> [l]
    | Node (_, subtrees) -> List.fold_left (fun acc subtree -> 
      acc @ (parse_tree_leaves subtree)) [] subtrees


(* 3. make_matcher gram that returns a matcher for the grammar gram *)
let rec make_matcher (init_symbol, rule_func) acc frag =

  (* mr (match rule) is a recursive function that attempts to match a 
  given rule (a sequence of grammar symbols) against the beginning of
  the fragment. It uses the accumulator function to handle the matched
  suffix of the fragment. *)
  let rec mr rule frag acc = 
    match rule with
    | [] -> acc frag
    | T symbol :: rest -> 
      (
      match frag with
      | h :: t -> if h = symbol then mr rest t acc else None
      | [] -> None
      )
    | N symbol :: rest ->
      let productions = rule_func symbol in
      mp productions frag (fun f -> mr rest f acc) 

    (* mp (match productions) is a helper function that attempts to match 
    any of the given productions against the fragment. It recursively 
    tries each production until one matches or all fail. *)
    and mp productions suffix acc = 
      match productions with
      | [] -> None
      | h :: t -> 
        let result = mr h suffix acc in
        match result with
        | None -> mp t suffix acc
        | Some _ -> result
      in
    (* Start matching from the productions of the initial symbol. *)
    let init_productions = rule_func init_symbol in
    mp init_productions frag acc


(* 4. make_parser gram that returns a parser for the grammar gram *)
(* loop_nonterminal tries to match a list of grammar rules (rule) against a
fragment (frag) using a given acceptor.It recursively processes each rule
and if a match is found, it constructs a list of matched rules. *)
let rec loop_nonterminal overall_gram rule acceptor frag = match rule with
  | [] -> None
  | hd::tl -> (match check_rule overall_gram hd acceptor frag with 
    | None -> loop_nonterminal overall_gram tl acceptor frag
    | Some r -> Some (hd::r))

(* check_rule tries to match a single rule against the fragment. If the
   rule is empty, it calls the acceptor. *)
and check_rule overall_gram rule acceptor frag = match rule with 
  | [] -> acceptor frag 
  | _ -> (match frag with 
    | [] -> None
    | hd::tl -> (match rule with
        |[] -> None
        |(T termn)::rem -> if hd = termn then (check_rule overall_gram rem 
                                                acceptor tl) else None
        |(N nontermn)::rem -> (loop_nonterminal overall_gram
                              (overall_gram nontermn)
                              (check_rule overall_gram rem acceptor) frag)));;

(* along_tree builds a parse tree from a list of rules starting
   from a root symbol. *)
let rec along_tree root rules = 
  match root with 
  | [] -> (rules, []) 
  | hd::tl -> (match (down_tree hd rules) with 
      | (a,b) -> (match along_tree tl a with 
          | (y,z) -> (y, b::z))) 
  and down_tree root rules =
  match root with 
  | (T cur) -> (match rules with 
      | [] -> ([], Leaf cur) 
      | hd::tl -> (hd::tl, Leaf cur)) 
      | (N cur) -> (match rules with 
          | [] -> ([], Node (cur, [])) 
          | hd::tl -> (match along_tree hd tl with 
              | (a,b) -> (a, Node (cur, b))));; 
  
  (* empty is an acceptor function that returns a successful match 
     only if the remaining suffix is empty. *)
  let empty suffix = match suffix with
  | [] -> Some []
  | _ -> None;;
  
  (* make_parser constructs a parser for a given grammar, which attempts
     to parse a fragment into a parse tree. *)
  let make_parser gram = match gram with
  | (startExp, grammar) -> fun frag -> match (loop_nonterminal grammar 
                                      (grammar startExp) empty frag) with
  | None -> None 
  | Some [] -> None 
  | Some x -> (match along_tree [N startExp] x with 
      | (_,t) -> (match t with 
          | [] -> None 
          | hd::tl -> Some hd));;

