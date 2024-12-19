type player = {
  name : string;
  position : string;
  shooting : int;
  finishing : int;
  athleticism : int;
  playmaking : int;
  defense : int;
  rebounding : int;
}

module PlayerSet : sig
  type t = player list
  (** The set type is a list of players *)

  val empty : t
  (** [empty] Initialize an empty set *) 
  val add : player -> t -> t
  (** [add player players] is a fucntion that adds adds a player to the set of players if not already present *)
  val remove : string -> t -> t
  (** [remove player_name players] is a function that removes a player by name *)
  val mem : string -> t -> bool
  (** [mem player_name players] checks if a player with the same name already exists in
     the set  *)

  val pop5 : t -> t * t
  
  val preview : t -> string
  (** [preview players] previews all players in the set *)
  val to_list : t -> player list
  (** [to_list players] returns a list of all players *)

  val append : t -> t -> t
  (** [append players new_players] returns a list of all players with new added players *)

end

val load_players : string -> PlayerSet.t
(** [load_players file_path] loads players from a CSV file into a PlayerSet *)
val parse_player_row : string list -> player
(** [parse_player_row row] parses a row from a CSV of players *)


(* Expose validation functions *)
val valid_positions : string list
val is_valid_position : string -> bool
val is_valid_stat : int -> bool
val validate_player : player -> bool