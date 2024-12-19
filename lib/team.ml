open Player
open PlayerAlgo

type team = {
  name : string;
  players : PlayerSet.t;
  overall_rating : int;
}

(** Helper Function to calculate team overal rating based on players rating *)
let calculate_team_rating (players : PlayerSet.t) : int =
  int_of_float (PlayerAlgo.team_overall players)

module Team = struct
  type t = team

  let name team = team.name
  let players team = team.players
  let overall_rating team = team.overall_rating

  let create_team name players =
    let overall_rating = calculate_team_rating players in
    { name; players; overall_rating }

  let preview team =
    let team_info =
      Printf.sprintf "Team: %s\nOverall Rating: %d\nPlayers:\n" team.name
        team.overall_rating
    in
    let player_info = PlayerSet.preview team.players in
    team_info ^ player_info
end
