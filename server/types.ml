(** Request and response types for the IMDB lookup service *)

(** Single query for a title lookup *)
type query = {
  title: string;
  year: int option; [@default None]
  title_types: string list option; [@default None]
} [@@deriving yojson]

(** Single match result *)
type search_result = {
  title: string;
  year: int option;
  imdb_rating: float;
  imdb_id: string;
  title_type: string;
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
