(** Rating backend selection.
    Change this module to switch between OMDb API and local rateflix server. *)

(* let fetch_imdb_rating = Omdb.fetch_imdb_rating *)
let fetch_imdb_rating = Rateflix_client.fetch_imdb_rating
