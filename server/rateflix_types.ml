type title_type =
  | TvPilot [@name "tvPilot"]
  | TvShort [@name "tvShort"]
  | VideoGame [@name "videoGame"]
  | TvSpecial [@name "tvSpecial"]
  | TvMiniSeries [@name "tvMiniSeries"]
  | TvMovie [@name "tvMovie"]
  | TvSeries [@name "tvSeries"]
  | Video [@name "video"]
  | Movie [@name "movie"]
  | Short [@name "short"]
  | TvEpisode [@name "tvEpisode"]
  | Unknown [@name "unknown"]
[@@deriving yojson, bin_io]

(* We should find a deriver that can make this table *)
let title_type_of_string = function
  | "tvPilot" -> TvPilot
  | "tvShort" -> TvShort
  | "videoGame" -> VideoGame
  | "tvSpecial" -> TvSpecial
  | "tvMiniSeries" -> TvMiniSeries
  | "tvMovie" -> TvMovie
  | "tvSeries" -> TvSeries
  | "video" -> Video
  | "movie" -> Movie
  | "short" -> Short
  | "tvEpisode" -> TvEpisode
  | _ -> Unknown

(** Request and response types for the IMDB lookup service *)

(** Single query for a title lookup *)
type query = {
  title: string;
  year: int option; [@default None]
  title_types: title_type list option; [@default None]
} [@@deriving yojson]

(** Single match result *)
type search_result = {
  title: string;
  year: int option;
  imdb_rating: float;
  imdb_id: string;
  title_type: title_type;
  match_score: float;
} [@@deriving yojson]

(** Batch request - list of queries *)
type request = query list [@@deriving yojson]

(** Batch response - list of (query, result) pairs *)
type response = (query * search_result) list [@@deriving yojson]

(** Exceptions for error handling *)
exception Invalid_request of string
exception Data_not_loaded
exception Parse_error of string
