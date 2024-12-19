open Team

type game = {
  home_team : Team.t;
  away_team : Team.t;
  home_score : int;
  away_score : int;
}
(* Player stats can mayber us associative list. *)

type team_stats = {
  team : Team.t;
  wins : int;
  losses : int;
  points_for : int;
  points_against : int;
}

type season = {
  games : game list;
  standings : team_stats list;
}

(* Access game type fields. *)
let home_team_score (game : game) : int = game.home_score
let away_team_score (game : game) : int = game.away_score

let game_score (game : game) : string =
  let home_name = Team.name game.home_team in
  let away_name = Team.name game.away_team in
  let h_score = game.home_score in
  let a_score = game.away_score in
  let winner = if h_score > a_score then home_name else away_name in
  "Game: " ^ home_name ^ " v.s " ^ away_name ^ "\n" ^ "Home Team: " ^ home_name
  ^ " : " ^ "Away Team: " ^ away_name ^ "\n" ^ "Score: " ^ string_of_int h_score
  ^ " to " ^ string_of_int a_score ^ "\n" ^ "Winner: " ^ winner

(* Access team_stats type fields. *)
let team_name_of_stats (team : team_stats) : string = Team.name team.team
let team_wins (team : team_stats) : int = team.wins
let team_losses (team : team_stats) : int = team.losses
let team_points_for (team : team_stats) : int = team.points_for
let team_points_against (team : team_stats) : int = team.points_against

(* create initial team_stats type. *)
let create_team_stats team w l p_for p_against =
  { team; wins = w; losses = l; points_for = p_for; points_against = p_against }

(* create initial season type. *)
let create_season games standings = { games; standings }

(* Create initial game type. *)
let create_game h_team a_team h_score a_score =
  {
    home_team = h_team;
    away_team = a_team;
    home_score = h_score;
    away_score = a_score;
  }

(* Access season type fields *)
let season_games season = season.games
let season_standing season = season.standings

(* main functionalities below *)
let simulate_game (home_team : Team.t) (away_team : Team.t) : game =
  let rec vaild_score h_team a_team =
    let home_score =
      Random.int_in_range
        ~min:(Team.overall_rating home_team - 5)
        ~max:(Team.overall_rating home_team + 20)
    in
    let away_score =
      Random.int_in_range
        ~min:(Team.overall_rating away_team - 20)
        ~max:(Team.overall_rating away_team + 20)
    in
    if home_score <> away_score then
      { home_team; away_team; home_score; away_score }
    else vaild_score h_team a_team
  in
  vaild_score home_team away_team

let update_stats (stats : team_stats list) (game : game) : team_stats list =
  List.map
    (fun stat ->
      if stat.team = game.home_team then
        {
          stat with
          wins = (stat.wins + if game.home_score > game.away_score then 1 else 0);
          losses =
            (stat.losses + if game.home_score <= game.away_score then 1 else 0);
          points_for = stat.points_for + game.home_score;
          points_against = stat.points_against + game.away_score;
        }
      else if stat.team = game.away_team then
        {
          stat with
          wins = (stat.wins + if game.away_score > game.home_score then 1 else 0);
          losses =
            (stat.losses + if game.away_score <= game.home_score then 1 else 0);
          points_for = stat.points_for + game.away_score;
          points_against = stat.points_against + game.home_score;
        }
      else stat)
    stats

let generate_schedule (teams : Team.t list) : (Team.t * Team.t) list =
  let all_games =
    List.concat_map
      (fun home_team ->
        List.map
          (fun away_team -> (home_team, away_team))
          (List.filter (fun t -> t <> home_team) teams))
      teams
  in
  (* Shuffle the games to make the schedule random *)
  let shuffled_games = List.sort (fun _ _ -> Random.int 3 - 1) all_games in
  (* Take the first 82 games for each team *)
  let rec pick_games games team_counts =
    match games with
    | [] -> []
    | (home_team, away_team) :: rest ->
        let home_count = List.assoc home_team team_counts in
        let away_count = List.assoc away_team team_counts in
        if home_count < 41 && away_count < 41 then
          (home_team, away_team)
          :: pick_games rest
               ((home_team, home_count + 1)
               :: (away_team, away_count + 1)
               :: List.remove_assoc home_team
                    (List.remove_assoc away_team team_counts))
        else pick_games rest team_counts
  in
  let initial_counts = List.map (fun t -> (t, 0)) teams in
  pick_games shuffled_games initial_counts

let play_season (schedule : (Team.t * Team.t) list) : game list =
  List.map
    (fun (home_team, away_team) -> simulate_game home_team away_team)
    schedule

let preview_standings (stats : team_stats list) : string =
  List.fold_left
    (fun acc stat ->
      let entry =
        Printf.sprintf "Team: %s, Wins: %d, Losses: %d, PF: %d, PA: %d\n"
          (Team.name stat.team) stat.wins stat.losses stat.points_for
          stat.points_against
      in
      acc ^ entry)
    "" stats

let standings_updater_aux (team_statistics : team_stats list) (game : game)
    (live : bool) : team_stats list =
  let result = update_stats team_statistics game in
  if live then print_endline (preview_standings result) else ();
  Unix.sleepf 0.02;
  result

let compute_standings (teams : Team.t list) (games : game list) (live : bool) :
    team_stats list =
  let initial_stats =
    List.map
      (fun team ->
        { team; wins = 0; losses = 0; points_for = 0; points_against = 0 })
      teams
  in
  List.fold_left
    (fun stats game -> standings_updater_aux stats game live)
    initial_stats games

let simulate_season (teams : Team.t list) (live : bool) : season =
  let schedule = generate_schedule teams in
  let games = play_season schedule in
  let standings = compute_standings teams games live in
  { games; standings }

let team_rankings (season : season) : string =
  let sorted_teams =
    List.sort (fun t1 t2 -> compare t2.wins t1.wins) season.standings
  in
  List.mapi
    (fun i stat ->
      Printf.sprintf "%d. %s - Wins: %d, Losses: %d" (i + 1)
        (Team.name stat.team) stat.wins stat.losses)
    sorted_teams
  |> String.concat "\n"

let season_to_list (season : season) = season.standings
let get_team_from_stats (stats : team_stats) = stats.team

let sort_standings team_stats =
  List.sort (fun t1 t2 -> compare t2.wins t1.wins) team_stats
