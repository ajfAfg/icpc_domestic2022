(*********************
 * My_int
 *********************)
module My_int : sig
  type t = int

  val add : t -> t -> t
  val comb : t -> t -> t
end = struct
  type t = int

  let order = 998_244_353
  let add x y = (x + y) mod order

  (* NOTE: For n, k in the dataset, since n * k ≤ 100_000_000. *)
  let memo = Hashtbl.create 100_000_000

  let rec comb n k =
    let key = (n, k) in
    match Hashtbl.find_opt memo key with
    | Some ans -> ans
    | None ->
        let ans =
          if k = n || k = 0 then 1
          else add (comb (n - 1) (k - 1)) (comb (n - 1) k)
        in
        Hashtbl.add memo key ans;
        ans
end

(*********************
 * Dataset
 *********************)
module Dataset = struct
  type t = {
    (* 1 ≤ k ≤ n ≤ 10^4 *)
    n : int;
    k : int;
    (* List.sort ss = List.init n ((+) 1) *)
    ss : int list;
    (* List.sort ts = List.init n ((+) 1) *)
    ts : int list;
  }

  let rec parse_input = function
    | [] -> []
    | nk_str :: ss_str :: ts_str :: lines -> (
        match
          [ nk_str; ss_str; ts_str ]
          |> List.map @@ String.split_on_char ' '
          |> List.map @@ List.map int_of_string
        with
        | [ [ n; k ]; ss; ts ] -> { n; k; ss; ts } :: parse_input lines
        | _ -> raise Util.Invalid_input)
    | _ -> raise Util.Invalid_input
end

(*********************
  * Position
  *********************)
module Positions = Map.Make (Int)

(*********************
  * Main
  *********************)
let partition positions ss =
  let rec partition' acc positions = function
    | x :: x' :: xs -> (
        match acc with
        | y :: ys ->
            if Positions.find x positions < Positions.find x' positions then
              let acc = (x :: y) :: ys in
              partition' acc positions @@ (x' :: xs)
            else
              let acc = [] :: (x :: y) :: ys in
              partition' acc positions @@ (x' :: xs)
        | _ -> raise (Invalid_argument "partition"))
    | x :: [] -> (
        match acc with
        | y :: ys -> (x :: y) :: ys
        | _ -> raise (Invalid_argument "partition"))
    | _ -> acc
  in
  ss |> partition' [ [] ] positions |> List.map List.rev

let count_splittable_part xs =
  let xs_with_index = List.mapi (fun i x -> (i, x)) xs in
  List.fold_left
    (fun acc (i, x) ->
      acc
      + (Bool.to_int
        @@ List.fold_left
             (fun acc (j, y) -> if j < i then acc && y < x else acc)
             true xs_with_index))
    0
  @@ List.tl xs_with_index

let rec merge xs ys =
  match (xs, ys) with
  | [], [] -> []
  | [], ys -> ys
  | xs, [] -> xs
  | x :: xs', y :: ys' -> if x < y then x :: merge xs' ys else y :: merge xs ys'

let exists_solution k partitioned_ss ts =
  List.length partitioned_ss <= k && ts = List.fold_left merge [] partitioned_ss

let solve () =
  Util.read_input "0 0" |> Dataset.parse_input
  |> List.iter (fun ({ k; ss; ts; _ } : Dataset.t) ->
         let partitioned_ss =
           let positions =
             ts
             |> List.mapi (fun i x -> (i, x))
             |> List.fold_left
                  (fun acc (i, x) -> Positions.add x i acc)
                  Positions.empty
           in
           ss |> partition positions
         in

         let n =
           partitioned_ss
           |> List.map count_splittable_part
           |> List.fold_left ( + ) 0
         in
         let ans =
           if exists_solution k partitioned_ss ts then
             List.init (min n (k - List.length partitioned_ss) + 1) Fun.id
             |> List.map (My_int.comb n)
             |> List.fold_left My_int.add 0
           else 0
         in
         Printf.printf "%d\n" ans)
