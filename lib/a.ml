(*********************
 * Dataset
 *********************)
module Dataset = struct
  type t = {
    n : int; (* 3 ≤ n ≤ 1000 *)
    vs : int list;
        (* List.length vs = n ∧
           (∀ v ∈ vs. 0 ≤ v ≤ 1000) ∧
           v_1 = 0 ∧
           (∀ i ∈ N. 1 ≤ i < n → v_i ≠ v_{i+1}) *)
  }

  let rec parse_input = function
    | [] -> []
    | n_str :: line :: lines ->
        {
          n = int_of_string n_str;
          vs = line |> String.split_on_char ' ' |> List.map @@ int_of_string;
        }
        :: parse_input lines
    | _ -> raise Util.Invalid_input
end

(*********************
  * Main
  *********************)
let rec count_peak = function
  | v1 :: v2 :: v3 :: vs ->
      Bool.to_int (v1 < v2 && v2 > v3) + count_peak (v2 :: v3 :: vs)
  | _ -> 0

let solve () =
  Util.read_input "0" |> Dataset.parse_input
  |> List.iter (fun ({ vs; _ } : Dataset.t) ->
         Printf.printf "%d\n" @@ count_peak vs)
