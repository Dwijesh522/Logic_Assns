open Formula;;
open Bdd;;

(*return variable string from indices*)
let name i j = String.concat "" ["c"; string_of_int(i); string_of_int(j)];;

(* atleast one queen in rth row*)
let rec atleastOneInARow r j=
  if j == 0 then Program.Variable(name r j)
  else Program.OprBinary(Program.OR, (atleastOneInARow r (j-1)), Program.Variable(name r j))
;;

(* atleast one queen in each row*)
let rec atleastOneInAllRow i n=
  if i == 0 then (atleastOneInARow i n)
  else Program.OprBinary(Program.AND, atleastOneInAllRow (i-1) n, (atleastOneInARow i n))
;;

(* not of all except i, j in a row*)
let rec notOfAllInRowExcept n r c j =
  if j == c then (
    if (j ==0) then Program.Constant(true)
    else notOfAllInRowExcept n r c (j-1)
  )
  else if j == 0 then Program.OprUnary(Program.NOT, Program.Variable(name r j))
  else
    Program.OprBinary(Program.AND, (notOfAllInRowExcept n r c (j-1)), Program.OprUnary(Program.NOT, Program.Variable(name r j)))
;;

(*exactly one in a cell*)
let exactlyOneInACell n r c=
  Program.OprBinary(Program.IFTHEN, Program.Variable(name r c), notOfAllInRowExcept n r c n)
;;

(*exactly one in given row*)
let rec exactlyOneInARow n r j=
  if j == 0 then exactlyOneInACell n r j
  else Program.OprBinary(Program.AND, exactlyOneInARow n r (j-1), exactlyOneInACell n r j)
;;

(* exactly one in all row *)
let rec exactlyOneInAllRows n i=
  if i == 0 then exactlyOneInARow n i n
  else Program.OprBinary(Program.AND, exactlyOneInAllRows n (i-1), exactlyOneInARow n i n)
;;

(* if c(r, c) is true then all elements in col are false except for given r*)
let rec notOfAllInColExcept n r c i=
  if i == r then (
    if (i==0) then Program.Constant(true)
    else notOfAllInColExcept n r c (i-1)
  )
  else if i == 0 then Program.OprUnary(Program.NOT, Program.Variable(name i c))
  else Program.OprBinary(Program.AND, notOfAllInColExcept n r c (i-1), Program.OprUnary(Program.NOT, Program.Variable(name i c)))
;;

(*if cij (for all i) is true then all elements in col are false*)
let exactlyOneInACellCol n c r=
  Program.OprBinary(Program.IFTHEN, Program.Variable(name r c), notOfAllInColExcept n r c n)
;;

(*exactly one in a col*)
let rec exactlyOneInACol n c i =
  if i == 0 then exactlyOneInACellCol n c i
  else Program.OprBinary(Program.AND, exactlyOneInACol n c (i-1), exactlyOneInACellCol n c i)
;;

(*exactly one in all cols*)
let rec exactlyOneInAllCols n j=
  if j == 0 then exactlyOneInACol n j n
  else Program.OprBinary(Program.AND, exactlyOneInAllCols n (j-1), exactlyOneInACol n j n)
;;

(*not of all elements in backword diagonal except (r, c)th*)
let rec notOfAllInBackDiaExcept n r c k=
  let x = k in
  let y = c+k-r in
  if r == k then (
    if x == 0 || y == 0 then Program.Constant(true)
    else  notOfAllInBackDiaExcept n r c (k-1)
  )
  else (
    if x == 0 || y == 0 then (
      (* no further recursive call possible*)
      Program.OprUnary(Program.NOT, Program.Variable(name x y))
    )
    else (
      (*recursive call possible*)
      if x <= n && y <= n && x >= 0 && y >= 0 then (*within the board*)
        Program.OprBinary(Program.AND, notOfAllInBackDiaExcept n r c (k-1), Program.OprUnary(Program.NOT, Program.Variable(name x y)))
      else notOfAllInBackDiaExcept n r c (k-1)
    )
  )
;;

(*not of all elements in forward diagonal except (r, c)th*)
let rec notOfAllInForwardDiaExcept n r c k=
  let x = k in
  let y = r+c-k in
  if r == k then (
    if x == 0 || y == n then Program.Constant(true)
    else  notOfAllInForwardDiaExcept n r c (k-1)
  )
  else (
    if x == 0 || y == n then (
      (* no further recursive call possible*)
      Program.OprUnary(Program.NOT, Program.Variable(name x y))
    )
    else (
      (*recursive call possible*)
      if x <= n && y <= n && x >= 0 && y >= 0 then (*within the board*)
        Program.OprBinary(Program.AND, notOfAllInForwardDiaExcept n r c (k-1), Program.OprUnary(Program.NOT, Program.Variable(name x y)))
      else notOfAllInForwardDiaExcept n r c (k-1)
    )
  )
;;

(*crc -> and not other backward diagonal elements*)
let backwardDia n r c=
  Program.OprBinary(Program.IFTHEN, Program.Variable(name r c), notOfAllInBackDiaExcept n r c n)
;;

(*crc -> and not other forward diagonal elements*)
let forwardDia n r c=
  Program.OprBinary(Program.IFTHEN, Program.Variable(name r c), notOfAllInForwardDiaExcept n r c n)
;;

(*iterate all cols*)
let rec loopCols n r j=
  if j==0 then Program.OprBinary(Program.AND, forwardDia n r j, backwardDia n r j)
  else Program.OprBinary(Program.AND, loopCols n r (j-1), Program.OprBinary(Program.AND, forwardDia n r j, backwardDia n r j))
;;

(* iterate all rows*)
let rec loopRows n i=
  if i==0 then loopCols n i n
  else Program.OprBinary(Program.AND, loopRows n (i-1), loopCols n i n)
;;

let rec loopColsOrdering n r j=
  if j == 0 then [name r j]
  else (name r j) :: loopColsOrdering n r (j-1)
;;

(* append two lists *)
let rec append l1 l2 =
  match l1 with
  | []    ->  l2
  | h::t  ->  h :: append t l2
;;

(*iterate rows for ordering*)
let rec loopRowsOrdering n i=
  if i == 0 then loopColsOrdering n i n
  else append (loopRowsOrdering n (i-1)) (loopColsOrdering n i n)
;;

exception No_Satisfying_Solution_Exists

let n_queen board_size =
  let n = board_size-1 in
  let e1 = atleastOneInAllRow n n in
  let e2 = exactlyOneInAllRows n n in
  let e3 = exactlyOneInAllCols n n in
  let e4 = loopRows n n in
  let e12 = Program.OprBinary(Program.AND, e1, e2) in
  let e34 = Program.OprBinary(Program.AND, e3, e4) in
  let t = Program.OprBinary(Program.AND, e12, e34) in
  let order = loopRowsOrdering n n in
  let robdd = BDD.bddFromExpr t order in
  (*BDD.to_dot robdd;*)
  (*flush stdout;*)
  BDD.any_sat robdd
;;

let rec print l=
  match l with
  |[] ->  print_string "\n"
  |h::t ->  Printf.printf "%s " h;
            print t
;;

(* !! REMOVE THIS IN SUBMISSION !!*)
(*
    n_queen 4;;
*)
(*
    let a = Program.Variable("a");;
    let b = Program.Variable("b");;
    let c = Program.Variable("c");;
    let d = Program.Variable("d");;
    let e = Program.Variable("e");;
 
    let ab = Program.OprBinary(Program.AND, a, b);;
    let _a = Program.OprUnary(Program.NOT, a);;
    let _b = Program.OprUnary(Program.NOT, b);;
    let _c = Program.OprUnary(Program.NOT, c);;
    let _d = Program.OprUnary(Program.NOT, d);;
    let a_b = Program.OprBinary(Program.AND, a, _b);;
    let _ce = Program.OprBinary(Program.AND, _c, e);;
    let _de = Program.OprBinary(Program.AND, _d, e);;

    let t1 = Program.OprBinary(Program.OR, _a, a_b);;
    let t2 = Program.OprBinary(Program.OR, _de, d);;
    let t3 = Program.OprBinary(Program.AND, c, t2);;
    let t4 = Program.OprBinary(Program.OR, _ce, t3);;
    let t5 = Program.OprBinary(Program.AND, t1, t4);;

    let t = Program.OprBinary(Program.OR, ab, t5);;

    let order2 = ["e"; "c"; "d"; "a"; "b"];;
    let robdd = BDD.bddFromExpr t order2;;
    BDD.to_dot robdd;;
*)
(*
    let a = Program.Variable("a");;
    let b = Program.Variable("b");;
    let c = Program.Variable("c");;

    let ac = Program.OprBinary(Program.AND, a, c);;
    let _cb = Program.OprBinary(Program.AND, Program.OprUnary(Program.NOT, c), b);;
    let t = Program.OprBinary(Program.OR, ac, _cb);;
    let order = ["a"; "b"; "c"];;
    let robdd = BDD.bddFromExpr t order;;
    BDD.to_dot robdd;;
*)

(*	The type of  knight is int -> int -> int -> string list, where
	first int is board size, second and third ints represent the
	row and column number (starting from 0) respectively of 
	initial position of the knight on the board
	The output to this function should be a sequence of strings.
	The first element in the sequence should be cell name 
	corresponding to the initial position of the knight. 
	Each subsequent string in the sequence should represent the 
	next cell visited by the knight. 
*)
let knight board_size init_row init_col = raise BDD.Not_implemented ;;	

