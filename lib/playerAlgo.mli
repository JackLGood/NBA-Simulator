open Player

val position_weights : string -> int * int * int * int * int * int
(** [position_weights position] returns a tuples that models the importance of
    certian attributes based on the position. *)

val player_rating : player -> string * string * int
(** [player_rating player] calculates a [player] rating based on it's
    position-specific weight and attributes. *)

val position_distribution : int -> int -> int -> int -> int -> float
(** [position_distribution pg sg sf pf c] return a metric to calculate the
    stength of a team based on their player position distribution. *)

val team_ratings_with_positions : ('a * string * int) list -> float
(** [team_ratings_with_positions rated_players] calculates the team rating based
    on a list of players [rated_players] that all are already rated. *)

val team_overall : PlayerSet.t -> float
(** [team_overall players] calculates the overall of a team based on its
    [players]. *)
