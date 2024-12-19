open Team

type game
type team_stats
type season

val home_team_score : game -> int
(** [home_team_score game] returns the number of points the home team scored
    during the [game]. The function allows access to record field home_team from
    type game. *)

val away_team_score : game -> int
(** [home_team_score game] returns the number of points the away team scored
    during the [game]. The function allows access to record field away_team from
    type game. *)

val game_score : game -> string
(** [game_score game] returns a string representation of the game score,
    highlighting the teams, score and winner. *)

val team_name_of_stats : team_stats -> string
(** [team_name_of_stats team_stats] returns the name of the of the team that the
    [team_stats] holds stats on. *)

val team_wins : team_stats -> int
(** [team_wins team_stats] returns the the number of wins the team [teams_stats]
    holds stats on. The function allows access to the record field wins of type
    team_stats. *)

val team_losses : team_stats -> int
(** [team_losses team_stats] returns the the number of losses the team
    [teams_stats] holds stats on. The function allows access to the record field
    losses of type team_stats. *)

val team_points_for : team_stats -> int
(** [team_points_for team_stats] returns the the number of points scored over
    all games the team [teams_stats] holds stats on. The function allows access
    to the record field points_for of type team_stats. *)

val team_points_against : team_stats -> int
(** [team_points_against team_stats] returns the the number of points other
    teams have scored over all games the team [teams_stats] holds stats on. The
    function allows access to the record field points_against of type
    team_stats. *)

val create_team_stats : Team.t -> int -> int -> int -> int -> team_stats
(** [create_team_stats team w l p_for p_against] creates the type team_stats
    based on arguments [team w l p_for p_against]. *)

val create_season : game list -> team_stats list -> season
(** [create_season games standings] creates the type season based on arguments
    based on arguments [game standings]. [standings] is not ordered. *)

val create_game : Team.t -> Team.t -> int -> int -> game
(** [create_game h_team a_team h_score a_score] creates the type game based on
    arguments [h_team a_team h_score a_score]. *)

val season_games : season -> game list
(** [season_games season] returns the list of games played throughout the
    season. The function allows access to the record field games of type season. *)

val season_standing : season -> team_stats list
(** [season_standing season] returns the list of of team_stats to simualate the
    team standings in the season. The function allows access to the record
    standing of type season. Standings is not ordered. *)

val simulate_game : Team.t -> Team.t -> game
(** [simulate_game home_team away_team] simulates a game between 
  [home_team away_team]. Scores of the game are calculated a random. However 
  a higher team overall and home court advantage can help with winning the game. 
  *)

val update_stats : team_stats list -> game -> team_stats list
(** [update_stats stats game] updates the stats of teams involved in the game 
  accordingly to the outcome of the game. *)

val generate_schedule : Team.t list -> (Team.t * Team.t) list
(** [generate_schedule teams] generates a schedules for all the list of teams in
  [teams]. *)

val play_season : (Team.t * Team.t) list -> game list
(** [play_season schedule] returns of list of all the games played based on 
  [schedule]. *)

val compute_standings : Team.t list -> game list -> bool -> team_stats list
(** [compute_standings teams games] computes the standings of the season based 
  on the list of teams [teams] and list of the games played [games]. *)
val simulate_season : Team.t list -> bool -> season
(** [simulate_season teams] simulates a season based on the list of teams in 
  the seaosn. Returns type season with all the games played and calculated 
  standings. Standing is not in order. *)

val preview_standings : team_stats list -> string
(** [preview_standings season] returns a string representation of the standings 
  of the season. *)

val team_rankings : season -> string
(** [team_rankings season] returns a string representation of [season] standings
    and it is ordered. *)
 val season_to_list : season -> team_stats list
(** [season_to_list season] returns a list of standings. *)

val get_team_from_stats : team_stats -> Team.t
(** [get_team_from_stats stats] returns the team in a statline. *)
val sort_standings : team_stats list -> team_stats list
(** [sort_standings stats] returns the sorted standings of the league. *)
