open Player

module Team : sig
  type t

  val name : t -> string
  (** [name team] returns the name of teame*)

  val players : t -> PlayerSet.t
  (** [players team] returns the playerSet of team*)

  val overall_rating : t -> int
  (** [overall_rating team] returns the overall_rating of team *)

  val create_team : string -> PlayerSet.t -> t
  (** [create_team name players] creates a team with team name [name], set of
      players [players] and calculates the team overall_rating based on players
      individuall statistics*)

  val preview : t -> string
  (** [preview team] previews team name and it's overall rating *)
end
