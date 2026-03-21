(** Pure request handlers *)

type state = {
  index: Database.t;
}

(** Lookup a single query - pure function called by workers *)
let lookup_one state (query : Types.query) : Types.search_result * Database.search_stats option =
  match Database.search state.index ?year:query.year ~title_types:query.title_types query.title with
  | None ->
      { Types.title = query.title;
        year = query.year;
        imdb_rating = 0.0;
        imdb_id = "";
        title_type = Types.Unknown;
        match_score = 0.0 }, None
  | Some (entry, score, stats) ->
      { Types.title = entry.Imdb_data.primary_title;
        year = entry.Imdb_data.year;
        imdb_rating = entry.Imdb_data.rating;
        imdb_id = entry.Imdb_data.tconst;
        title_type = entry.Imdb_data.title_type;
        match_score = score }, Some stats
