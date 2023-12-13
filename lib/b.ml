(*********************
 * List
 *********************)
module List = struct
  include List

  let partition n (* 0 ≤ n *) xs =
    let xs, ys =
      xs
      |> List.mapi (fun i x -> (i, x))
      |> List.partition (fun (i, _) -> i < n)
    in
    let remove_index = List.map (fun (_, x) -> x) in
    (remove_index xs, remove_index ys)

  let min = function
    | [] -> raise (Invalid_argument "min")
    | x :: xs -> List.fold_left (fun acc x -> min acc x) x xs
end

(*********************
 * Dataset
 *********************)
module Dataset = struct
  type t = {
    n : int; (* 2 ≤ n ≤ 1000 *)
    ccs : int list list;
        (* List.length ccs = n ∧
           (∀ x ∈ N. 1 ≤ x ≤ n →
            ccs |> List.flatten |> List.filter ((=) x) |> List.length |> (=) 2) *)
  }

  let rec parse_input = function
    | [] -> []
    | n_str :: lines ->
        let n = int_of_string n_str in
        let lines1, lines2 = List.partition n lines in
        {
          n;
          ccs =
            lines1
            |> List.map @@ String.split_on_char ' '
            |> List.map @@ List.map int_of_string;
        }
        :: parse_input lines2
end

(*********************
  * Main
  *********************)
(* NOTE: A Simulation by revursive function causes stack overflow. *)
let play_game ({ n; ccs } : Dataset.t) =
  (* NOTE: Pre-processing *)
  let ccs = ccs |> Array.of_list in
  for i = 0 to n - 1 do
    ccs.(i) <-
      (if ccs.(i) |> List.sort_uniq compare |> List.length |> ( = ) 1 then []
      else ccs.(i))
  done;

  (* NOTE: Main processing *)
  let players =
    List.init n Fun.id
    |> List.filter (fun player -> ccs.(player) <> [])
    |> List.to_seq |> Queue.of_seq
  in
  let ans = ref 0 in

  while not @@ Queue.is_empty players do
    (* NOTE: Take a card from `player`'s cards. *)
    let player = Queue.take players in
    let card = List.min ccs.(player) in
    ccs.(player) <- List.filter (( <> ) card) ccs.(player);

    if ccs.(player) <> [] then Queue.push player players;

    (* NOTE: Put `card` into `next_player`'s cards. *)
    let next_player = Queue.peek players in
    ccs.(next_player) <-
      (if List.mem card ccs.(next_player) then
       List.filter (( <> ) card) ccs.(next_player)
      else card :: ccs.(next_player));

    if ccs.(next_player) = [] then ignore @@ Queue.take players;

    incr ans
  done;
  !ans

let solve () =
  Util.read_input "0" |> Dataset.parse_input
  |> List.iter (fun dataset -> Printf.printf "%d\n" @@ play_game dataset)
