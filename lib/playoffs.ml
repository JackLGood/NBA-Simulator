include RegularSeason
open Team
open PlayerAlgo

let get_max_player p1 p2 =
  let name1, pos1, rat1 = p1 in
  let name2, pos2, rat2 = p2 in
  if rat1 >= rat2 then p1 else p2

let get_star_player players =
  match players with
  | [] -> failwith "Empty list"
  | h :: t -> List.fold_left (fun acc elt -> get_max_player h acc) h t

let get_star_mult star =
  let _, _, rating = star in
  let luck_factor = rating mod 3 in
  match luck_factor with
  | 0 -> 1.03
  | 2 -> 0.97
  | _ -> 1.

let simulate_game (home_team : Team.t) (away_team : Team.t) : game =
  let rec vaild_score h_team a_team =
    let home_score_base =
      Random.int_in_range
        ~min:(Team.overall_rating home_team - 5)
        ~max:(Team.overall_rating home_team + 15)
    in
    let away_score_base =
      Random.int_in_range
        ~min:(Team.overall_rating away_team - 10)
        ~max:(Team.overall_rating away_team + 10)
    in
    let home_players = List.map player_rating (Team.players home_team) in
    let away_players = List.map player_rating (Team.players away_team) in
    let home_mult = home_players |> get_star_player |> get_star_mult in
    let away_mult = away_players |> get_star_player |> get_star_mult in
    let home_score = int_of_float (float_of_int home_score_base *. home_mult) in
    let away_score = int_of_float (float_of_int away_score_base *. away_mult) in
    if home_score <> away_score then
      create_game home_team away_team home_score away_score
    else vaild_score h_team a_team
  in
  vaild_score home_team away_team

let rec take n lst =
  match lst with
  | [] -> []
  | _ when n <= 0 -> []
  | x :: xs -> x :: take (n - 1) xs

let get_playoff_teams (season : season) =
  let teams = season_to_list season in
  let sorted = RegularSeason.sort_standings teams in
  let playoff_teams = take 16 sorted in
  let res = List.map (fun x -> get_team_from_stats x) playoff_teams in
  res

let initialize_bracket (teams : Team.t list) =
  let is_even_length lst = List.length lst mod 2 = 0 in

  let split_list lst =
    let rec split acc1 acc2 = function
      | [] -> (List.rev acc1, List.rev acc2)
      | [ x ] -> (List.rev (x :: acc1), List.rev acc2)
      | x :: y :: rest -> split (x :: acc1) (y :: acc2) rest
    in
    split [] [] lst
  in

  let reverse_list lst =
    let rec rev acc = function
      | [] -> acc
      | h :: t -> rev (h :: acc) t
    in
    rev [] lst
  in

  let rec pair_teams front back acc =
    match (front, back) with
    | [], [] -> List.rev acc
    | f :: fs, b :: bs -> pair_teams fs bs ((Some f, Some b) :: acc)
    | _, _ -> failwith "Unexpected mismatch in list lengths"
  in

  if not (is_even_length teams) then
    failwith "The list of teams must have an even number of elements"
  else
    let front_half, back_half = split_list teams in
    let reversed_back_half = reverse_list back_half in
    pair_teams front_half reversed_back_half []

let rec make_bracket teams =
  match teams with
  | [] -> []
  | h1 :: h2 :: t -> (Some h1, Some h2) :: make_bracket t
  | _ -> failwith "The list of teams must have an even number of elements"

let simulate_helper z =
  match z with
  | Some x2, Some y2 ->
      if Team.overall_rating x2 > Team.overall_rating y2 then x2 else y2
  | _ -> failwith "The list of teams must have an even number of elements"

let simulate_round (rnd : (Team.t option * Team.t option) list) :
    (Team.t option * Team.t option) list =
  match rnd with
  | [ (h, _) ] -> [ (h, None) ]
  | _ -> make_bracket (List.map simulate_helper rnd)
