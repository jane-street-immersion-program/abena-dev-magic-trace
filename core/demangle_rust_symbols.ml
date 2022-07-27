open! Core

let did_llvm_add_extra_period_deliminting_words (suffix : string) =
  let check_if_suffix_is_empty = String.( = ) suffix "" in
  let does_suffix_start_with_period = String.is_prefix suffix ~prefix:"." in
  let suffix_to_char_list = String.to_list suffix in
  let ( <= ) = Char.( <= ) in
  let is_suffix_symbol_like =
    List.for_all suffix_to_char_list ~f:(fun character ->
      Char.is_alphanum character
      || ('\x21' <= character && character <= '\x2F')
      || ('\x41' <= character && character <= '\x5A')
      || ('\x61' <= character && character <= '\x7A')
      || ('\x30' <= character && character <= '\x39'))
  in
  (not check_if_suffix_is_empty) && does_suffix_start_with_period && is_suffix_symbol_like
;;

let demangle_symbol input =
  let decoded_symbol = Demangle_rust_legacy_symbols.demangle input in
  match decoded_symbol with
  | Ok { Demangle_rust_legacy_symbols.body; suffix } ->
    (match did_llvm_add_extra_period_deliminting_words suffix with
     | true -> Ok (body ^ suffix)
     | false -> Ok body)
  | Error _ -> Error "fail"
;;
