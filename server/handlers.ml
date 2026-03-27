(** Pure request handlers *)

type state = {
  index: Database.t;
}

(** Lookup a single query - pure function called by workers *)
let lookup_one state (query : Rateflix_types.query) : Rateflix_types.search_result * Database.search_stats option =
  match Database.search state.index ?year:query.year ~title_types:query.title_types query.title with
  | None ->
      { Rateflix_types.title = query.title;
        year = query.year;
        imdb_rating = 0.0;
        imdb_id = "";
        title_type = Rateflix_types.Unknown;
        match_score = 0.0 }, None
  | Some (title_match, entry, score, stats) ->
      let title = match title_match with
        | Database.Primary -> entry.Imdb_data.primary_title
        | Database.Secondary -> entry.Imdb_data.secondary_title
      in
      { Rateflix_types.title;
        year = entry.Imdb_data.year;
        imdb_rating = entry.Imdb_data.rating;
        imdb_id = Printf.sprintf "tt%07d" entry.Imdb_data.tconst;
        title_type = entry.Imdb_data.title_type;
        match_score = score }, Some stats
