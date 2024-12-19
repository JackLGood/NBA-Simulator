open Finalproject.Player
open Finalproject.Team
open Finalproject.Playoffs
(* let get_game_probabilites t1_rating t2_rating : int * int = let tot =
   t1_rating + t2_rating in (t1_rating/tot, t2_rating/tot) *)

let _ = Random.self_init ()
let player_set = load_players "data/nba_players.csv"

let menu_border =
  "o--------------------------------------------------------------------o\n"

let menu_line text border =
  let border_length =
    String.length border - 3 (* Exclude the border characters *)
  in
  let text_length = String.length text in
  if text_length > border_length then
    failwith "Text is too long for the provided border."
  else
    let padding = (border_length - text_length) / 2 in
    let left_padding = String.make padding ' ' in
    let right_padding =
      String.make (border_length - text_length - padding) ' '
    in
    "|" ^ left_padding ^ text ^ right_padding ^ "|\n"

let rec home_screen () =
  print_string menu_border;
  print_string (menu_line "Welcome to OCaml NBA Simulator 2024:" menu_border);
  print_string (menu_line "" menu_border);
  print_string (menu_line "Press 'p' to play now!" menu_border);
  print_string (menu_line "OR" menu_border);
  print_string
    (menu_line "Press 'h' to learn about supported features and how to play!"
       menu_border);
  print_string menu_border;
  let input = read_line () |> String.lowercase_ascii in
  match input with
  | "p" ->
      print_endline "Starting the game...";
      () (* Proceed to the main functionality of your program *)
  | "h" ->
      print_string menu_border;
      print_string (menu_line "Game Features:" menu_border);
      print_string (menu_line "" menu_border);
      print_endline
        "|       - Add players: Customize your roster by adding current       |\n\
         |         or your very own custom players with attributes like       |\n\
         |         shooting, finishing, etc.                                  |\n\
         |       - Create teams: Join a current NBA team or create a new      |\n\
         |          francise and take control of your team's roster.          |\n\
         |       - Preview teams: View the rosters of cuurent teams.          |\n\
         |       - Simulate tournament: Watch teams compete in a bracket      |\n\
         |         style tournament until a winner is crowned.                |";
      print_string (menu_line "" menu_border);
      print_string
        (menu_line "Are you ready to start playing? Press 'p' to play now!"
           menu_border);
      print_string menu_border;
      let next_input = read_line () |> String.lowercase_ascii in
      if next_input = "p" then (
        print_endline "Starting the game...";
        () (* Proceed to the main functionality of your program *))
      else (
        print_endline "Invalid input. Returning to the home screen...";
        home_screen ())
  | _ ->
      print_endline
        "Invalid input. Please type 'p' to play or 'h' for more information. \n";
      home_screen () (* Retry for invalid input *)

let () = home_screen ()

(* Function to add players *)
let rec add_players player_set =
  print_string menu_border;
  print_string (menu_line "Do you want to add a player? (yes/no)" menu_border);
  print_string menu_border;
  let response = read_line () |> String.lowercase_ascii in
  if response = "yes" then
    let () =
      print_string menu_border;
      print_string (menu_line "Enter player name:" menu_border);
      print_string menu_border
    in
    let name = read_line () in
    let rec get_valid_position () =
      print_endline "Enter player position (pg, sg, sf, pf, c):";
      let position = read_line () |> String.lowercase_ascii in
      if is_valid_position position then position
      else (
        print_endline "Invalid position";
        get_valid_position ())
    in
    let position = get_valid_position () in
    let rec get_valid_stat stat_name =
      print_endline ("Enter " ^ stat_name ^ " (0-100):");
      let stat_str = read_line () in
      try
        let stat = int_of_string stat_str in
        if is_valid_stat stat then stat
        else (
          print_endline "Invalid stat value.";
          get_valid_stat stat_name)
      with Failure _ ->
        print_endline "Invalid input. Please enter an integer.";
        get_valid_stat stat_name
    in
    let shooting = get_valid_stat "shooting" in
    let finishing = get_valid_stat "finishing" in
    let athleticism = get_valid_stat "athleticism" in
    let playmaking = get_valid_stat "playmaking" in
    let defense = get_valid_stat "defense" in
    let rebounding = get_valid_stat "rebounding" in
    let player =
      {
        name;
        position;
        shooting;
        finishing;
        athleticism;
        playmaking;
        defense;
        rebounding;
      }
    in
    if PlayerSet.mem name player_set then (
      print_endline "Player with this name already exists. Player not added.";
      add_players player_set)
    else
      let player_set =
        if validate_player player then PlayerSet.add player player_set
        else (
          print_endline "Invalid player data. Player not added.";
          player_set)
      in
      add_players player_set
  else if response = "no" then player_set
  else (
    print_endline "Invalid response. Please type 'yes' or 'no'.";
    add_players player_set)

let player_set = PlayerSet.append player_set (add_players player_set)

let rec make_teams playerset n teams =
  if n = 0 then (teams, playerset)
  else
    let team, remaining_players = PlayerSet.pop5 playerset in
    make_teams remaining_players (n - 1) (team :: teams)

let split_into_teams playerset =
  let teams, remaining_players = make_teams playerset 30 [] in
  (teams, remaining_players)

let shuffle_list lst =
  let arr = Array.of_list lst in
  let len = Array.length arr in

  for i = len - 1 downto 1 do
    let j = Random.int (i + 1) in
    let temp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- temp
  done;

  Array.to_list arr

let nba_team_names =
  shuffle_list
    [
      "Lakers";
      "Warriors";
      "Celtics";
      "Bulls";
      "Spurs";
      "Heat";
      "Knicks";
      "Nets";
      "76ers";
      "Clippers";
      "Rockets";
      "Mavericks";
      "Suns";
      "Magic";
      "Pacers";
      "Pistons";
      "Kings";
      "Raptors";
      "Hawks";
      "Hornets";
      "Timberwolves";
      "Bucks";
      "Thunder";
      "Jazz";
      "Wizards";
      "Pelicans";
      "Cavaliers";
      "Trail Blazers";
      "Grizzlies";
      "Nuggets";
    ]

let split_teams, _ = split_into_teams player_set

let tournament_teams =
  List.map2
    (fun name team -> Team.create_team name team)
    nba_team_names split_teams

(* Ask user if they want to preview the teams *)
let rec ask_preview teams =
  print_string menu_border;
  print_string
    (menu_line "Do you want a preview of the teams? (yes/no)" menu_border);
  print_string menu_border;
  let response = read_line () |> String.lowercase_ascii in
  if response = "yes" then
    List.iter
      (fun t ->
        print_endline (Team.preview t);
        print_endline "")
      teams
  else if response = "no" then ()
  else (
    print_endline "Invalid response. Please type 'yes' or 'no'.";
    ask_preview teams)

let () = ask_preview tournament_teams

(* Helper function to get a single stat from the user *)
let rec get_stat stat_name =
  Printf.printf "Enter new %s (0-100): " stat_name;
  let input = read_line () in
  try
    let value = int_of_string input in
    if value >= 0 && value <= 100 then value
    else (
      print_endline "Stat must be between 0 and 100.";
      get_stat stat_name)
  with _ ->
    print_endline "Invalid input. Please enter an integer.";
    get_stat stat_name

(* Helper function to get all updated stats for a player *)
let get_updated_stats_for_player player =
  let new_shooting = get_stat "shooting" in
  let new_finishing = get_stat "finishing" in
  let new_athleticism = get_stat "athleticism" in
  let new_playmaking = get_stat "playmaking" in
  let new_defense = get_stat "defense" in
  let new_rebounding = get_stat "rebounding" in
  {
    player with
    shooting = new_shooting;
    finishing = new_finishing;
    athleticism = new_athleticism;
    playmaking = new_playmaking;
    defense = new_defense;
    rebounding = new_rebounding;
  }

(* Helper function to find and update a player's stats on a given team *)
let update_player_on_team (team : Team.t) player_name =
  let players = Team.players team |> PlayerSet.to_list in
  match
    List.find_opt
      (fun p ->
        String.lowercase_ascii p.name = String.lowercase_ascii player_name)
      players
  with
  | None ->
      print_endline "Player not found.";
      None
  | Some player ->
      let updated_player = get_updated_stats_for_player player in
      let old_players = Team.players team in
      let new_players =
        old_players
        |> PlayerSet.remove player.name
        |> PlayerSet.add updated_player
      in
      let updated_team = Team.create_team (Team.name team) new_players in
      Some updated_team

let rec update_player_stats (teams : Team.t list) : Team.t list =
  print_string menu_border;
  print_string
    (menu_line "Do you want to update a player's stats? (yes/no)" menu_border);
  print_string menu_border;
  let response = read_line () |> String.lowercase_ascii in
  match response with
  | "yes" -> (
      print_string menu_border;
      print_string (menu_line "Enter the team name:" menu_border);
      print_string menu_border;
      let team_name = read_line () in
      match
        List.find_opt
          (fun t ->
            String.lowercase_ascii (Team.name t)
            = String.lowercase_ascii team_name)
          teams
      with
      | None ->
          print_endline "Team not found.";
          update_player_stats teams
      | Some team -> (
          print_string menu_border;
          print_string
            (menu_line "Enter the player's name to update:" menu_border);
          print_string menu_border;
          let player_name = read_line () in
          match update_player_on_team team player_name with
          | None ->
              (* No update performed, just recursively try again *)
              update_player_stats teams
          | Some updated_team ->
              (* Replace the old team with the updated one in the list *)
              let rec replace_team lst =
                match lst with
                | [] -> []
                | h :: t ->
                    if
                      String.lowercase_ascii (Team.name h)
                      = String.lowercase_ascii team_name
                    then updated_team :: t
                    else h :: replace_team t
              in
              let updated_teams = replace_team teams in
              print_string menu_border;
              print_string
                (menu_line "Player stats updated successfully!" menu_border);
              print_string menu_border;
              (* After updating, prompt again if they want to continue
                 updating *)
              update_player_stats updated_teams))
  | "no" ->
      (* User is done updating; return the current list of teams *)
      teams
  | _ ->
      print_endline "Invalid response. Please type 'yes' or 'no'.";
      update_player_stats teams

let tournament_teams = update_player_stats tournament_teams
let rec repeat_string s n = if n <= 0 then "" else s ^ repeat_string s (n - 1)

let print_matchup ((t, s) : Team.t option * Team.t option) =
  match (t, s) with
  | Some t1, Some s1 ->
      let str1 =
        Team.name t1 ^ " "
        ^ repeat_string "-" (19 - String.length (Team.name t1))
      in
      let str2 =
        Team.name s1 ^ " "
        ^ repeat_string "-" (19 - String.length (Team.name s1))
      in
      print_endline str1;
      print_endline
        (String.sub (repeat_string "                    |\n" 4) 0 (88 - 1));
      print_endline str2
  | _ -> failwith "huh"

let print_round (tourney : (Team.t option * Team.t option) list) =
  match tourney with
  | [ (Some h, None) ] -> print_endline ("Tournament Winner: " ^ Team.name h)
  | _ ->
      List.iter
        (fun x ->
          print_matchup x;
          print_endline "")
        tourney

let rec simulate_tournament rnd round_number =
  print_endline ("\nRound " ^ string_of_int round_number ^ ":\n");
  print_round rnd;
  if
    List.length rnd = 1
    && List.exists (fun (x, y) -> Option.is_some x && Option.is_none y) rnd
  then
    (* Final round has a winner *)
    match rnd with
    | [ (Some _, None) ] -> rnd (* Return the final round *)
    | _ -> failwith "Unexpected state: Tournament final round malformed"
  else (
    print_string menu_border;
    print_string
      (menu_line "Press Enter to continue to the next round..." menu_border);
    print_string menu_border;
    ignore (read_line ());
    (* Wait for the user to press Enter *)
    simulate_tournament
      (Finalproject.Playoffs.simulate_round rnd)
      (round_number + 1)
    (* Recursively simulate the next round *))

(* let print_tournament (t : (Team.t option * Team.t option) list list) =
   List.iter (fun x -> print_round x) t *)

let () =
  let _ =
    print_string menu_border;
    print_string (menu_line "Press Enter to simulate the Season!" menu_border);
    print_string menu_border;
    read_line ()
  in
  let season =
    Finalproject.RegularSeason.simulate_season tournament_teams true
  in
  let _ =
    print_string menu_border;
    print_string
      (menu_line "Season Complete! Press Enter to see the Standings!"
         menu_border);
    print_string menu_border;
    read_line ()
  in
  let standings = Finalproject.RegularSeason.team_rankings season in
  let () =
    print_endline standings;
    print_endline ""
  in
  let _ =
    print_string menu_border;
    print_string (menu_line "Press Enter to start the playoffs!" menu_border);
    print_string menu_border;
    read_line ()
  in
  let playoff_teams = Finalproject.Playoffs.get_playoff_teams season in
  let bracket = Finalproject.Playoffs.initialize_bracket playoff_teams in
  let final_round = simulate_tournament bracket 1 in
  match final_round with
  | [ (Some winner, None) ] ->
      print_string menu_border;
      print_string (menu_line "" menu_border);
      print_string
        (menu_line
           ("Congratulations to the " ^ Team.name winner ^ "!")
           menu_border);
      print_string (menu_line "" menu_border);
      print_endline
        "|                                         .''.                       |\n\
         |             .''.      .        *''*    :_\\/_:     .                |\n\
         |            :_\\/_:   _\\(/_  .:.*_\\/_*   : /\\ :  \
         .'.:.'.             |\n\
         |        .''.: /\\ :    /)\\   ':'* /\\ *  : '..'.  \
         -=:o:=-             |\n\
         |       :_\\/_:'.:::.  | ' *''*    * \
         '.\\'/.'_\\(/_'.':'.'              |\n\
         |       : /\\ : :::::  =  *_\\/_*     -= o =- /)\\    '  \
         ***            |\n\
         |        '..'  ':::' === * /\\ *     .'/.\\'.  ' \
         ._____                |\n\
         |            *        |   *..*         :       |.   |' .---’|        |\n\
         |              *      |     _           .--'|  ||   | _|    |        |\n\
         |              *      |  .-'|       __  |   |  |    ||      |        |\n\
         |           .-----.   |  |' |  ||  |  | |   |  |    ||      |        |\n\
         |        ___'       ' /- |  '-.’’.    '-'   '-.'    '`      |____    |\n\
         |      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~      |\n\
         |       -~                    ~-~-~-~-~-~-~-~-~-~   /|               |\n\
         |       ~-~-~-~-~        ~-~-~-~-~-~-~-~  /|~       /_|              |\n\
         |               __-~__  -~-~-~-~-~-~     /_|    -~======-~           |\n\
         |       ~-~-~-~~-~-~--~     ~-~-~-~     /__|_ ~-~-~-~                |\n\
         |      ~-~-~-~-~-~    ~-~~-~-~-~-~    ========  ~-~-~-~              |";
      print_string (menu_line "" menu_border);
      print_string menu_border
  | _ -> failwith "Unexpected state: No winner found"
