open Player

(* Define weights for different positions *)
let position_weights position =
  match position with
  | "PG" ->
      (3, 1, 2, 4, 1, 1)
      (* shooting, finishing, athleticism, playmaking, defense, rebounding *)
  | "SF" -> (2, 2, 2, 2, 2, 2)
  | "SG" -> (4, 1, 2, 4, 1, 1)
  | "PF" -> (1, 3, 3, 1, 2, 2)
  | "C" -> (1, 1, 2, 2, 2, 4)
  | _ -> failwith "Not a position"

(* Calculate player rating based on position-specific weights *)
let player_rating (player : player) : string * string * int =
  let ( shooting_w,
        finishing_w,
        athleticism_w,
        playmaking_w,
        defense_w,
        rebounding_w ) =
    position_weights player.position
  in
  let weighted_score =
    (player.shooting * shooting_w)
    + (player.finishing * finishing_w)
    + (player.athleticism * athleticism_w)
    + (player.playmaking * playmaking_w)
    + (player.defense * defense_w)
    + (player.rebounding * rebounding_w)
  in
  let rating = weighted_score / 12 in
  (player.name, player.position, rating)

let position_distribution pg sg sf pf c =
  match (pg, sg, sf, pf, c) with
  | 1, 1, 1, 1, 1 -> 1.01 (* Balanced *)
  | 1, 1, 1, _, _ -> 1.02 (* Mixed Bigs *)
  | 0, 0, 0, _, _ -> 0.84 (* No perimeter presence *)
  | _, 0, 0, _, _ -> 0.91 (* No perimeter defense *)
  | _, _, 0, 0, 0 -> 0.89 (* No interior presence *)
  | _, _, _, 0, 0 -> 0.92 (* No interior defense *)
  | 0, 0, _, _, _ -> 0.90 (* No true guard *)
  | _, 2, 2, _, 0 -> 1.07 (* Positionless *)
  | 2, 1, 0, _, _ -> 1.03 (* Lob City *)
  | 1, 2, _, _, _ -> 1.06 (* Threes *)
  | 1, _, 3, _, 1 | 1, _, 3, 1, _ -> 0.95 (* Raptors *)
  | 1, _, _, 2, 1 -> 1.02 (* Stretch bigs *)
  | 0, _, _, 2, 1 -> 0.94 (* Stretch bigs, no ball movement *)
  | _, _, _, _, _ -> 1.

let team_ratings_with_positions rated_players =
  let count_position pos =
    List.length (List.filter (fun (_, p, _) -> p = pos) rated_players)
  in
  let pg = count_position "PG" in
  let sg = count_position "SG" in
  let sf = count_position "SF" in
  let pf = count_position "PF" in
  let c = count_position "C" in
  let distribution_mult = position_distribution pg sg sf pf c in
  let init_rating =
    List.fold_left (fun acc (_, _, rating) -> rating + acc) 0 rated_players
  in
  let rating =
    init_rating
    / if List.length rated_players > 0 then List.length rated_players else 1
  in
  float_of_int rating *. distribution_mult

let team_overall players =
  let list_players = PlayerSet.to_list players in
  let rated_players = List.map player_rating list_players in
  let rating = team_ratings_with_positions rated_players in
  rating
