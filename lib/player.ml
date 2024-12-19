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

module PlayerSet = struct
  type t = player list

  let empty : t = []

  let mem player_name (players : t) =
    List.exists (fun p -> p.name = player_name) players

  let add (player : player) (players : t) : t =
    if mem player.name players then players else player :: players

  let remove player_name (players : t) : t =
    List.filter (fun p -> p.name <> player_name) players

  let preview (players : t) : string =
    List.map
      (fun p -> Printf.sprintf "Name: %s, Position: %s" p.name p.position)
      players
    |> String.concat "\n"

  let pop5 (players : t) : t * t =
    match players with
    | h1 :: h2 :: h3 :: h4 :: h5 :: t -> ([ h1; h2; h3; h4; h5 ], t)
    | _ -> failwith "Not enough players"

  let to_list (players : t) : player list = players
  let append (players : t) (new_players : t) = players @ new_players
end

let parse_player_row row =
  match row with
  | [
   name;
   position;
   shooting;
   finishing;
   athleticism;
   playmaking;
   defense;
   rebounding;
  ] ->
      {
        name;
        position;
        shooting = int_of_string shooting;
        finishing = int_of_string finishing;
        athleticism = int_of_string athleticism;
        playmaking = int_of_string playmaking;
        defense = int_of_string defense;
        rebounding = int_of_string rebounding;
      }
  | _ -> failwith "Unexpected CSV format"

let load_players file_path =
  let csv = Csv.load file_path in
  List.fold_right
    (fun row acc ->
      let player = parse_player_row row in
      PlayerSet.add player acc)
    csv PlayerSet.empty

let valid_positions = [ "pg"; "sg"; "sf"; "pf"; "c" ]

let is_valid_position pos =
  List.mem (String.lowercase_ascii pos) valid_positions

let is_valid_stat stat = stat >= 0 && stat <= 100

let validate_player player =
  is_valid_position player.position
  && is_valid_stat player.shooting
  && is_valid_stat player.finishing
  && is_valid_stat player.athleticism
  && is_valid_stat player.playmaking
  && is_valid_stat player.defense
  && is_valid_stat player.rebounding
