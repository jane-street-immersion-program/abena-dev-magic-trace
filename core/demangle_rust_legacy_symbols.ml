open! Core

(* Create a list of the escape sequences. Make them a constant. The list will be a string * string list. *)
module Constants = struct
  let escape_sequences =
    [ "$C$", ","
    ; "$SP$", "@"
    ; "$BP$", "*"
    ; "$RF$", "&"
    ; "$LT$", "<"
    ; "$GT$", ">"
    ; "$LP$", "("
    ; "$RP$", ")"
    ]
  ;;
end

let decode_two_digit_hexadecimal_number first_character second_character =
  let%bind.Option hex_digit_of_first_character = Char.get_hex_digit first_character in
  let%bind.Option hex_digit_of_second_character = Char.get_hex_digit second_character in
  let leftshift_first_character = Int.shift_left hex_digit_of_first_character 4 in
  let bit_or_on_the_hexadecimals =
    Int.bit_or leftshift_first_character hex_digit_of_second_character
  in
  match Char.of_int bit_or_on_the_hexadecimals with
  | Some integer -> Some (Char.to_string integer)
  | None -> None
;;

let strip_llvm_parser =
  let open Angstrom in
  let open Angstrom.Let_syntax in
  let check_for_hex_or_at t = Char.is_hex_digit t || Char.( = ) t '@' in
  let normal_character = any_char >>| fun character -> Some (Char.to_string character) in
  many1
    (normal_character
    <* string ".llvm." *> skip_while check_for_hex_or_at *> end_of_input
    <|> normal_character)
;;

let is_rust_hash =
  let open Angstrom in
  string "h" *> skip_while Char.is_hex_digit *> at_end_of_input
;;

let get_integers_from_symbol_parser =
  let open Angstrom in
  let open Angstrom.Let_syntax in
  let is_digit = Char.is_digit in
  let int_of_string = take_while1 is_digit in
  let%bind string_to_int = int_of_string in
  let integer_in_symbol = Int.of_string string_to_int in
  take integer_in_symbol
;;

let splits_symbol_parser =
  let open Angstrom in
  let open Angstrom.Let_syntax in
  let remove_ZN = string "_ZN" *> return None in
  let remove__ZN = string "__ZN" *> return None in
  let removeZN = string "ZN" *> return None in
  let prefix =
    choice ~failure_msg:"unrecognized prefix" [ remove_ZN; remove__ZN; removeZN ]
  in
  let%bind first_half_of_parser =
    prefix *> many1 get_integers_from_symbol_parser <* char 'E'
  in
  let%bind second_half_of_parser = take_while (fun _ -> true) in
  return (first_half_of_parser, second_half_of_parser)
;;

let decode_parser =
  let open Angstrom in
  let open Angstrom.Let_syntax in
  let unescape_characters =
    List.map Constants.escape_sequences ~f:(fun (escape_sequence, new_char) ->
      string escape_sequence *> return (Some new_char))
  in
  let remove_first_underscore =
    [ (peek_string 2
      >>= fun first_two_characters ->
      match String.equal first_two_characters "_$" with
      | false -> fail "No _$ found"
      | true -> advance 1 *> return None)
    ]
  in
  let periods_to_semicolon = [ (string ".." >>| fun _ -> Some "::") ] in
  let two_digit_hexadecimal_number =
    let hex_character = satisfy Char.is_hex_digit in
    let hexcode =
      decode_two_digit_hexadecimal_number
      <$> string "$u" *> hex_character
      <*> hex_character
      <* string "$"
    in
    [ (hexcode
      >>= fun integer ->
      match integer with
      | None -> fail "invalid integer"
      | Some character -> return (Some character))
    ]
  in
  let normal_character =
    [ (any_char >>| fun character -> Some (Char.to_string character)) ]
  in
  let token =
    choice
      ~failure_msg:"unrecognized token"
      (remove_first_underscore
      @ unescape_characters
      @ two_digit_hexadecimal_number
      @ periods_to_semicolon
      @ normal_character)
  in
  let tokens =
    many1 token
    >>| fun tokens -> tokens |> List.filter_map ~f:Fn.id |> String.concat |> Some
  in
  tokens
;;

type t =
  { body : string
  ; suffix : string
  }

let remove_llvm_suffix mangled_symbol =
  let remove_llvm_parser =
    Angstrom.parse_string ~consume:All strip_llvm_parser mangled_symbol
  in
  let new_mangled_symbol =
    match remove_llvm_parser with
    | Ok symbol -> List.filter_map symbol ~f:Fn.id
    | Error reason -> [ reason ]
  in
  String.concat new_mangled_symbol
;;

let demangle mangled_symbol =
  let open Result.Let_syntax in
  let new_mangled_symbol = remove_llvm_suffix mangled_symbol in
  let%bind split_strings, suffix =
    Angstrom.parse_string ~consume:All splits_symbol_parser new_mangled_symbol
  in
  let filter_hash =
    List.filteri split_strings ~f:(fun index element ->
      let is_last_split = index + 1 = List.length split_strings in
      let is_split_a_hash = Angstrom.parse_string ~consume:All is_rust_hash element in
      let check_for_hash =
        match is_split_a_hash with
        | Ok symbol -> symbol
        | Error _ -> false
      in
      not (is_last_split && check_for_hash))
  in
  let body =
    List.filter_map filter_hash ~f:(fun str ->
      match Angstrom.parse_string ~consume:All decode_parser str with
      | Ok symbol -> symbol
      | Error _ -> None)
    |> String.concat ~sep:"::"
  in
  return { body; suffix }
;;
