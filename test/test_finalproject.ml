open OUnit2
open Finalproject.Player
open Finalproject.Team
open Finalproject.RegularSeason
open Finalproject.Playoffs
open Finalproject.PlayerAlgo
open Finalproject.PlayerAlgo

(* TEST FOR PLAYER FUNCTIONALITIES *)
let test_load_players () =
  let players = load_players "../data/test_player.csv" in
  let players_list = PlayerSet.to_list players in

  (* Verify the loaded players *)
  assert_equal 2 (List.length players_list);

  (* Jordan Poole,C,100,100,100,100,100,100 *)
  let player1 = List.nth players_list 0 in
  assert_equal "Lebron James" player1.name;
  assert_equal "SG" player1.position;
  assert_equal 84 player1.shooting;
  assert_equal 82 player1.finishing;
  assert_equal 87 player1.athleticism;
  assert_equal 92 player1.playmaking;
  assert_equal 84 player1.defense;
  assert_equal 75 player1.rebounding;

  let player2 = List.nth players_list 1 in
  assert_equal "Jordan Poole" player2.name;
  assert_equal "C" player2.position;
  assert_equal 100 player2.shooting;
  assert_equal 100 player2.finishing;
  assert_equal 100 player2.athleticism;
  assert_equal 100 player2.playmaking;
  assert_equal 100 player2.defense;
  assert_equal 100 player2.rebounding

let test_player_set () =
  let open PlayerSet in
  let empty_set = empty in
  let player1 =
    {
      name = "Player One";
      position = "SF";
      shooting = 70;
      finishing = 60;
      athleticism = 65;
      playmaking = 55;
      defense = 60;
      rebounding = 50;
    }
  in
  let player2 =
    {
      name = "Player Two";
      position = "C";
      shooting = 80;
      finishing = 70;
      athleticism = 75;
      playmaking = 65;
      defense = 80;
      rebounding = 90;
    }
  in
  let set1 = add player1 empty_set in
  let set2 = add player2 set1 in
  let set3 = remove player1.name set2 in
  assert_equal (List.length (to_list set2)) 2;
  assert_bool "Player One should be present" (mem player1.name set2);
  assert_bool "Player Two should be present" (mem player2.name set2);
  assert_equal (List.length (to_list (remove player1.name set2))) 1;
  assert_bool "Player One should be removed" (not (mem player1.name set3))

let test_player_fail () =
  let invalid_row = [ "John Doe"; "SG"; "85"; "78"; "90" ] in
  assert_raises (Failure "Unexpected CSV format") (fun () ->
      parse_player_row invalid_row)

let test_pop5 () =
  (* Create dummy player values for testing *)
  let p1 =
    {
      name = "Player 1";
      position = "PG";
      shooting = 80;
      finishing = 75;
      athleticism = 90;
      playmaking = 85;
      defense = 70;
      rebounding = 65;
    }
  in
  let p2 =
    {
      name = "Player 2";
      position = "SG";
      shooting = 85;
      finishing = 80;
      athleticism = 88;
      playmaking = 78;
      defense = 75;
      rebounding = 60;
    }
  in
  let p3 =
    {
      name = "Player 3";
      position = "SF";
      shooting = 70;
      finishing = 78;
      athleticism = 85;
      playmaking = 82;
      defense = 80;
      rebounding = 75;
    }
  in
  let p4 =
    {
      name = "Player 4";
      position = "PF";
      shooting = 60;
      finishing = 85;
      athleticism = 80;
      playmaking = 65;
      defense = 85;
      rebounding = 80;
    }
  in
  let p5 =
    {
      name = "Player 5";
      position = "C";
      shooting = 50;
      finishing = 90;
      athleticism = 75;
      playmaking = 60;
      defense = 90;
      rebounding = 85;
    }
  in
  let p6 =
    {
      name = "Player 6";
      position = "SF";
      shooting = 75;
      finishing = 70;
      athleticism = 80;
      playmaking = 77;
      defense = 72;
      rebounding = 68;
    }
  in

  (* Test with enough players *)
  let players = [ p1; p2; p3; p4; p5; p6 ] in
  let expected_popped = [ p1; p2; p3; p4; p5 ] in
  let expected_remaining = [ p6 ] in
  let appended_players = [ p1; p2 ] in
  let expected_append = [ p6; p1; p2 ] in

  let popped, remaining = PlayerSet.pop5 players in
  let appended = PlayerSet.append expected_remaining appended_players in
  assert_equal expected_popped popped;
  assert_equal expected_remaining remaining;
  assert_equal expected_append appended;

  (* Test with fewer than 5 players *)
  let insufficient_players = [ p1; p2; p3; p4 ] in
  assert_raises (Failure "Not enough players") (fun () ->
      PlayerSet.pop5 insufficient_players)

let test_validate_player () =
  let valid_player =
    {
      name = "Player 1";
      position = "pg";
      shooting = 80;
      finishing = 75;
      athleticism = 90;
      playmaking = 85;
      defense = 70;
      rebounding = 65;
    }
  in
  let invalid_position_player = { valid_player with position = "qb" } in
  let invalid_stat_player = { valid_player with shooting = 101 } in

  assert_bool "Valid player passes validation" (validate_player valid_player);
  assert_bool "Player with invalid position fails validation"
    (not (validate_player invalid_position_player));
  assert_bool "Player with invalid stat fails validation"
    (not (validate_player invalid_stat_player))

(* TEST FOR TEAM FUNCTIONALITIES *)
let test_nba_set =
  let open PlayerSet in
  let player1 =
    {
      name = "Lebron James";
      position = "SG";
      shooting = 84;
      finishing = 82;
      athleticism = 87;
      playmaking = 92;
      defense = 84;
      rebounding = 75;
    }
  in
  let player2 =
    {
      name = "Jordan Poole";
      position = "C";
      shooting = 100;
      finishing = 100;
      athleticism = 100;
      playmaking = 100;
      defense = 100;
      rebounding = 100;
    }
  in
  let set = add player2 empty in
  add player1 set

let test_team () =
  let players = load_players "../data/test_player.csv" in
  let team = Team.create_team "Team A" players in
  let team_preview = Team.preview team in
  let team_string = "Team: Team A\nOverall Rating: 96\nPlayers:\n" in
  let expected_player_preview = PlayerSet.preview test_nba_set in
  let combined_output = team_string ^ expected_player_preview in
  assert_equal "Team A" (Team.name team);
  assert_equal 96 (Team.overall_rating team);
  assert_equal test_nba_set (Team.players team);
  assert_equal ~printer:(fun x -> x) combined_output team_preview

let set_1 =
  let open PlayerSet in
  let player1 =
    {
      name = "Player One";
      position = "PG";
      shooting = 70;
      finishing = 60;
      athleticism = 65;
      playmaking = 55;
      defense = 60;
      rebounding = 50;
    }
  in
  let player2 =
    {
      name = "Player Two";
      position = "SG";
      shooting = 80;
      finishing = 70;
      athleticism = 75;
      playmaking = 65;
      defense = 80;
      rebounding = 90;
    }
  in
  let player3 =
    {
      name = "Player Two";
      position = "SF";
      shooting = 80;
      finishing = 70;
      athleticism = 75;
      playmaking = 65;
      defense = 80;
      rebounding = 90;
    }
  in
  let player4 =
    {
      name = "Player Two";
      position = "PF";
      shooting = 80;
      finishing = 70;
      athleticism = 75;
      playmaking = 65;
      defense = 80;
      rebounding = 90;
    }
  in
  let player5 =
    {
      name = "Player Two";
      position = "C";
      shooting = 80;
      finishing = 70;
      athleticism = 75;
      playmaking = 65;
      defense = 80;
      rebounding = 90;
    }
  in
  let set = add player1 empty in
  let set2 = add player2 set in
  let set3 = add player3 set2 in
  let set4 = add player4 set3 in
  add player5 set4

let set_2 =
  let open PlayerSet in
  let player1 =
    {
      name = "Player One";
      position = "PG";
      shooting = 70;
      finishing = 60;
      athleticism = 65;
      playmaking = 55;
      defense = 60;
      rebounding = 50;
    }
  in
  let player2 =
    {
      name = "Player Two";
      position = "SG";
      shooting = 90;
      finishing = 70;
      athleticism = 75;
      playmaking = 65;
      defense = 90;
      rebounding = 90;
    }
  in
  let player3 =
    {
      name = "Player Two";
      position = "SF";
      shooting = 80;
      finishing = 70;
      athleticism = 75;
      playmaking = 75;
      defense = 80;
      rebounding = 90;
    }
  in
  let player4 =
    {
      name = "Player Two";
      position = "PF";
      shooting = 80;
      finishing = 80;
      athleticism = 75;
      playmaking = 65;
      defense = 80;
      rebounding = 90;
    }
  in
  let player5 =
    {
      name = "Player Two";
      position = "C";
      shooting = 80;
      finishing = 70;
      athleticism = 75;
      playmaking = 75;
      defense = 90;
      rebounding = 90;
    }
  in
  let set = add player1 empty in
  let set2 = add player2 set in
  let set3 = add player3 set2 in
  let set4 = add player4 set3 in
  add player5 set4

(* TEST FOR REGULARSEASON FUNCTIONALITIES *)
let test_simulate_game_reg () =
  let team1 = Team.create_team "Team 1" set_1 in
  let team2 = Team.create_team "Team 2" set_2 in
  let game = Finalproject.RegularSeason.simulate_game team1 team2 in
  assert_bool "Home score within range"
    (home_team_score game >= Team.overall_rating team1 - 20
    && home_team_score game <= Team.overall_rating team1 + 20);
  assert_bool "Away score within range"
    (away_team_score game >= Team.overall_rating team2 - 20
    && away_team_score game <= Team.overall_rating team2 + 20);
  assert_bool "No ties allowed" (home_team_score game <> away_team_score game)

let test_update_stats () =
  let team = Team.create_team "Team 1" set_1 in
  let stats = [ create_team_stats team 0 0 0 0 ] in
  let game = create_game team team 100 90 in
  let updated_stats = update_stats stats game in
  let updated_team_stats = List.hd updated_stats in
  assert_equal (team_wins updated_team_stats) 1;
  assert_equal (team_losses updated_team_stats) 0;
  assert_equal (team_points_for updated_team_stats) 100;
  assert_equal (team_points_against updated_team_stats) 90

let team1 = Team.create_team "Team 1" set_1
let team2 = Team.create_team "Team 2" set_2
let team3 = Team.create_team "Team 3" set_1
let team4 = Team.create_team "Team 4" set_2
let team5 = Team.create_team "Team 5" set_1
let team6 = Team.create_team "Team 6" set_2
let team7 = Team.create_team "Team 7" set_1
let team8 = Team.create_team "Team 8" set_2
let team9 = Team.create_team "Team 9" set_1
let team10 = Team.create_team "Team 10" set_2
let team11 = Team.create_team "Team 11" set_1
let team12 = Team.create_team "Team 12" set_2
let team13 = Team.create_team "Team 13" set_1
let team14 = Team.create_team "Team 14" set_2
let team15 = Team.create_team "Team 15" set_1
let team16 = Team.create_team "Team 16" set_2
let team17 = Team.create_team "Team 17" set_1
let team18 = Team.create_team "Team 18" set_2
let team19 = Team.create_team "Team 19" set_1
let team20 = Team.create_team "Team 20" set_2
let team21 = Team.create_team "Team 21" set_2
let team22 = Team.create_team "Team 22" set_2
let team23 = Team.create_team "Team 23" set_2
let team24 = Team.create_team "Team 24" set_2
let team25 = Team.create_team "Team 25" set_2
let team26 = Team.create_team "Team 26" set_2
let team27 = Team.create_team "Team 27" set_2
let team28 = Team.create_team "Team 28" set_2
let team29 = Team.create_team "Team 29" set_2
let team30 = Team.create_team "Team 30" set_2

let teams_master1 =
  [
    team1;
    team2;
    team3;
    team4;
    team5;
    team6;
    team7;
    team8;
    team9;
    team10;
    team11;
    team12;
    team13;
    team14;
    team15;
    team16;
    team17;
    team18;
    team19;
    team20;
    team21;
    team22;
    team23;
    team24;
    team25;
    team26;
    team27;
    team28;
    team29;
    team30;
  ]

let teams_master2 =
  [
    team1;
    team2;
    team3;
    team4;
    team5;
    team6;
    team7;
    team8;
    team9;
    team10;
    team11;
    team12;
    team13;
    team14;
    team15;
    team16;
  ]

let test_generate_schedule () =
  let teams = teams_master1 in
  let schedule = generate_schedule teams in
  assert_equal (List.length schedule) 604 ~printer:string_of_int
(* expected is 604 which is less then total games played in the nba*)

let test_season_length () =
  let teams = [ team1; team2; team3; team4 ] in
  let season = simulate_season teams false in
  assert_equal
    (List.length (season_games season))
    (List.length teams * 3)
    ~printer:string_of_int

let test_standings () =
  let teams = [ team1; team2; team3; team4 ] in
  let season = simulate_season teams false in
  List.iter
    (fun stats ->
      assert_equal
        (team_wins stats + team_losses stats)
        6 ~printer:string_of_int) (* this one was 82 not 6*)
    (season_standing season)

let test_preview_standings () =
  let games = [ create_game team1 team2 80 90 ] in
  let team1_stats = create_team_stats team1 0 1 80 90 in
  let team2_stats = create_team_stats team2 1 0 90 80 in
  let season = create_season games [ team1_stats; team2_stats ] in
  let output =
    preview_standings (Finalproject.RegularSeason.season_to_list season)
  in
  let expected_output =
    "Team: Team 1, Wins: 0, Losses: 1, PF: 80, PA: 90\n\
     Team: Team 2, Wins: 1, Losses: 0, PF: 90, PA: 80\n"
  in
  assert_equal ~printer:(fun x -> x) expected_output output

let test_team_stats_field_access () =
  let team_stats = create_team_stats team1 1 0 101 70 in
  let output_name = team_name_of_stats team_stats in
  let output_wins = team_wins team_stats in
  let output_losses = team_losses team_stats in
  let output_points_for = team_points_for team_stats in
  let output_points_against = team_points_against team_stats in
  assert_equal ~printer:(fun x -> x) "Team 1" output_name;
  assert_equal 1 output_wins ~printer:string_of_int;
  assert_equal 0 output_losses ~printer:string_of_int;
  assert_equal 101 output_points_for ~printer:string_of_int;
  assert_equal 70 output_points_against ~printer:string_of_int;
  assert_equal team1 (get_team_from_stats team_stats)

let test_game_score1 () =
  let game = create_game team1 team2 80 90 in
  let output = game_score game in
  let expected_output =
    "Game: Team 1 v.s Team 2\n\
     Home Team: Team 1 : Away Team: Team 2\n\
     Score: 80 to 90\n\
     Winner: Team 2"
  in
  assert_equal ~printer:(fun x -> x) expected_output output

let test_game_score2 () =
  let game = create_game team1 team2 90 80 in
  let output = game_score game in
  let expected_output =
    "Game: Team 1 v.s Team 2\n\
     Home Team: Team 1 : Away Team: Team 2\n\
     Score: 90 to 80\n\
     Winner: Team 1"
  in
  assert_equal ~printer:(fun x -> x) expected_output output

let test_team_rankings () =
  let games = [ create_game team1 team2 80 90 ] in
  let team1_stats = create_team_stats team1 0 1 80 90 in
  let team2_stats = create_team_stats team2 1 0 90 80 in
  let season = create_season games [ team1_stats; team2_stats ] in
  let output = team_rankings season in
  let expected_output =
    "1. Team 2 - Wins: 1, Losses: 0\n2. Team 1 - Wins: 0, Losses: 1"
  in
  assert_equal ~printer:(fun x -> x) expected_output output;
  assert_equal team1 (get_team_from_stats team1_stats);
  assert_equal
    [ team2_stats; team1_stats ]
    (sort_standings [ team1_stats; team2_stats ])

(* Tests for playoffs *)
let test_simulate_game_yoffs () =
  let team1 = Team.create_team "Team 1" set_1 in
  let team2 = Team.create_team "Team 2" set_2 in
  let game = Finalproject.Playoffs.simulate_game team1 team2 in
  assert_bool "Home score within range"
    (home_team_score game >= Team.overall_rating team1 - 5
    && home_team_score game <= Team.overall_rating team1 + 15);
  assert_bool "Away score within range"
    (away_team_score game >= Team.overall_rating team2 - 10
    && away_team_score game <= Team.overall_rating team2 + 10);
  assert_bool "No ties allowed" (home_team_score game <> away_team_score game)

let test_get_playoff_teams () =
  let teams1 =
    [
      create_team_stats team1 7 0 0 0;
      create_team_stats team2 7 3 2 0;
      create_team_stats team3 7 1 4 2;
      create_team_stats team4 3 2 1 0;
      create_team_stats team5 2 0 3 4;
      create_team_stats team6 4 2 0 1;
      create_team_stats team7 3 4 1 3;
      create_team_stats team8 2 1 3 0;
      create_team_stats team9 3 0 2 4;
      create_team_stats team10 2 4 0 2;
      create_team_stats team11 2 2 4 1;
      create_team_stats team12 4 1 0 3;
      create_team_stats team13 2 3 1 0;
      create_team_stats team14 2 0 4 2;
      create_team_stats team15 1 4 0 1;
      create_team_stats team16 1 2 3 4;
      (* create_team_stats team17 0 1 2 0; *)
    ]
  in
  let szn = create_season [] teams1 in
  let playoff_teams = Finalproject.Playoffs.get_playoff_teams szn in
  assert_equal (List.length playoff_teams) 16 ~printer:string_of_int

let test_initialize_bracket () =
  let teams = teams_master2 in
  let init_bracket = Finalproject.Playoffs.initialize_bracket teams in
  let head1, head2 =
    match List.hd init_bracket with
    | Some v1, Some v2 -> (v1, v2)
    | _ -> failwith "Impossible"
  in
  let head3, head4 =
    match List.hd (List.tl init_bracket) with
    | Some v1, Some v2 -> (v1, v2)
    | _ -> failwith "Impossible"
  in
  assert_equal (List.length init_bracket) 8 ~printer:string_of_int;
  assert_equal (Team.name head1) "Team 1" ~printer:Fun.id;
  assert_equal (Team.name head2) "Team 16" ~printer:Fun.id;
  assert_equal (Team.name head3) "Team 3" ~printer:Fun.id;
  assert_equal (Team.name head4) "Team 14" ~printer:Fun.id

let test_uneven_league () =
  let teams = List.tl teams_master2 in
  let bracket =
    [
      (Some team1, Some team2);
      (Some team3, Some team4);
      (Some team5, Some team6);
      (Some team7, Some team8);
      (Some team9, Some team10);
      (Some team11, Some team12);
      (Some team13, Some team14);
      (Some team15, None);
    ]
  in
  assert_raises
    (Failure "The list of teams must have an even number of elements") (fun _ ->
      Finalproject.Playoffs.initialize_bracket teams);
  assert_raises
    (Failure "The list of teams must have an even number of elements") (fun _ ->
      Finalproject.Playoffs.simulate_round bracket)

let test_simulate_round () =
  let teams = teams_master2 in
  let init_bracket = Finalproject.Playoffs.initialize_bracket teams in
  let simmed = Finalproject.Playoffs.simulate_round init_bracket in
  let head1, head2 =
    match List.hd simmed with
    | Some v1, Some v2 -> (v1, v2)
    | _ -> failwith "Impossible"
  in
  assert_equal (List.length simmed) 4;
  assert_bool "first team is 1 or 16"
    (Team.name head1 = "Team 1" || Team.name head1 = "Team 16");
  assert_bool "second team is 3 or 14"
    (Team.name head2 = "Team 3" || Team.name head2 = "Team 14")

(* Player algo tests *)
let player_algo_test () =
  let position_combinations =
    [
      (1, 1, 1, 1, 1);
      (* Balanced *)
      (1, 1, 1, 2, 0);
      (* Mixed Bigs *)
      (0, 0, 0, 3, 2);
      (* No perimeter presence *)
      (1, 0, 0, 2, 2);
      (* No perimeter defense *)
      (2, 2, 0, 0, 0);
      (* No interior presence *)
      (1, 1, 3, 0, 0);
      (* No interior defense *)
      (0, 0, 2, 2, 1);
      (* No true guard *)
      (0, 2, 2, 1, 0);
      (* Positionless *)
      (2, 1, 0, 1, 1);
      (* Lob City *)
      (1, 2, 1, 1, 0);
      (* Threes *)
      (1, 0, 3, 0, 1);
      (* Raptors (first variant) *)
      (1, 0, 3, 1, 0);
      (* Raptors (second variant) *)
      (1, 1, 0, 2, 1);
      (* Stretch bigs *)
      (0, 1, 1, 2, 1);
      (* Stretch bigs, no ball movement *)
    ]
  in
  List.iter
    (fun x ->
      let x1, x2, x3, x4, x5 = x in
      assert_bool "Not 1."
        (Finalproject.PlayerAlgo.position_distribution x1 x2 x3 x4 x5 <> 1.))
    position_combinations

(* NEW TEST *)
let test_position_weights () =
  assert_equal (3, 1, 2, 4, 1, 1) (position_weights "PG");
  assert_equal (1, 1, 2, 2, 2, 4) (position_weights "C");
  assert_raises (Failure "Not a position") (fun () -> position_weights "QB")

let test_player_rating () =
  let player =
    {
      name = "John";
      position = "PG";
      shooting = 80;
      finishing = 70;
      athleticism = 60;
      playmaking = 90;
      defense = 50;
      rebounding = 40;
    }
  in
  assert_equal ("John", "PG", 73) (player_rating player)

let test_team_overall_three_players () =
  let players = PlayerSet.empty in
  let players =
    List.fold_left
      (fun acc player -> PlayerSet.add player acc)
      players
      [
        {
          name = "John";
          position = "PG";
          shooting = 80;
          finishing = 70;
          athleticism = 60;
          playmaking = 90;
          defense = 50;
          rebounding = 40;
        };
        {
          name = "Paul";
          position = "SG";
          shooting = 70;
          finishing = 80;
          athleticism = 90;
          playmaking = 60;
          defense = 40;
          rebounding = 50;
        };
        {
          name = "George";
          position = "C";
          shooting = 60;
          finishing = 70;
          athleticism = 80;
          playmaking = 50;
          defense = 90;
          rebounding = 100;
        };
      ]
  in
  let overall_rating = team_overall players in
  assert_equal 75.0 overall_rating ~printer:string_of_float

let tests =
  "All Priority Queue tests"
  >::: [
         ("Testing Player Set function" >:: fun _ -> test_player_set ());
         ("Testing load function for players" >:: fun _ -> test_load_players ());
         ("Testing invalid function" >:: fun _ -> test_player_fail ());
         ("Testing Player pop function" >:: fun _ -> test_pop5 ());
         ("Testing validation of player " >:: fun _ -> test_validate_player ());
         ("Testing Team fucntions" >:: fun _ -> test_team ());
         ( "Testing regularSeason fucntionality" >:: fun _ ->
           test_simulate_game_reg () );
         ("Testing team_stats functionality" >:: fun _ -> test_update_stats ());
         ( "Testing team_stats access of  fields" >:: fun _ ->
           test_team_stats_field_access () );
         ( "Testing generate schedule functionality" >:: fun _ ->
           test_generate_schedule () );
         ( "Testing season length functionality" >:: fun _ ->
           test_season_length () );
         ("Testing season standing functionality" >:: fun _ -> test_standings ());
         ( "Testing season preview standing functionality" >:: fun _ ->
           test_preview_standings () );
         ("Testing game score functionality" >:: fun _ -> test_game_score1 ());
         ("Testing game score functionqlity" >:: fun _ -> test_game_score2 ());
         ( "Testing team rankings functionality" >:: fun _ ->
           test_team_rankings () );
         ( "Testing simulate playoff game functionality" >:: fun _ ->
           test_simulate_game_yoffs () );
         ( "Testing get playoff teams functionality" >:: fun _ ->
           test_get_playoff_teams () );
         ( "Testing init bracket functionality" >:: fun _ ->
           test_initialize_bracket () );
         ( "Testing simulate round functionality" >:: fun _ ->
           test_simulate_round () );
         ( "Testing uneven league functionality" >:: fun _ ->
           test_uneven_league () );
         ( "Testing player position algo functionality" >:: fun _ ->
           player_algo_test () );
         ( "Testing position weights for playerAlgo" >:: fun _ ->
           test_position_weights () );
         ( "Testing player rating for playerAlgo" >:: fun _ ->
           test_player_rating () );
         ( "Testing team overall on a team with three players" >:: fun _ ->
           test_team_overall_three_players () );
       ]

let _ = run_test_tt_main tests
