open Team
open RegularSeason

val simulate_game : Team.t -> Team.t -> game
(** [simulate_game home_team away_team] simulates a game between [home_team] and
    [away_team]. Returns a [game] record containing the teams and their scores. *)

val get_playoff_teams : season -> Team.t list
(** [get_playoff_teams season teams] retrieves the top 16 teams from the given
    [season] based on their performance stats. Returns a list of [Team.t]. *)

val initialize_bracket : Team.t list -> (Team.t option * Team.t option) list
(** [make_bracket teams] takes a list of teams and pairs them into a bracket
    structure for playoffs. Returns a list of tuples, where each tuple
    represents a matchup between two teams. Raises [Failure] if the list of
    teams does not have an even number of elements. *)

val simulate_round :
  (Team.t option * Team.t option) list -> (Team.t option * Team.t option) list
(** [simulate_round] simulates one round of a tournament*)
