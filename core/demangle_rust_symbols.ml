open! Core

let is_char_punctuation character =
  let open Char in
  ('\x21' <= character && character <= '\x2F')
  || ('\x41' <= character && character <= '\x5A')
  || ('\x61' <= character && character <= '\x7A')
  || ('\x30' <= character && character <= '\x39')
;;

let did_llvm_add_extra_period_delimiting_words (suffix : string) =
  (* CR hlian: [is_suffix_empty], maybe? "check" is not a good name in a variable *)
  let check_if_suffix_is_empty = String.( = ) suffix "" in
  (* CR hlian: [suffix_starts_with_period], maybe? "does" is usually filler in a variable,
     since you can rewrite the question to be a present-tense sentence *)
  let does_suffix_start_with_period = String.is_prefix suffix ~prefix:"." in
  let is_suffix_symbol_like =
    suffix
    |> String.to_list
    |> List.for_all ~f:(fun character ->
         Char.is_alphanum character || is_char_punctuation character)
  in
  (not check_if_suffix_is_empty) && does_suffix_start_with_period && is_suffix_symbol_like
;;

let demangle_symbol input =
  let decoded_symbol = Demangle_rust_legacy_symbols.demangle input in
  match decoded_symbol with
  | Ok { Demangle_rust_legacy_symbols.body; suffix } ->
    (match did_llvm_add_extra_period_delimiting_words suffix with
     | true -> Ok (body ^ suffix)
     | false -> Ok body)
  | Error _ -> Error "fail"
;;