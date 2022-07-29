open! Core
open! Magic_trace_lib

(* Create a list of the escape sequences. Make them a constant. The list will be a string * char list. Create a parser that will make this a string
   string parser.t. Run on then parser by using *> on each parser and then turning it to the character. Then have a choice function that will return a singular string *)
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

let first_parser =
  let open Angstrom in
  let open Angstrom.Let_syntax in
  let number = Char.is_digit in
  let int_of_string = take_while1 number in
  let%bind string_to_int = int_of_string in
  let integer_in_string = Int.of_string string_to_int in
  print_s [%sexp (integer_in_string : int)];
  take integer_in_string
;;

let splits_parser =
  let open Angstrom in
  let remove_ZN = string "_ZN" *> return None in
  let remove__ZN = string "__ZN" *> return None in
  let removeZN = string "ZN" *> return None in
  (* let remove_first_underscore = string "_$" *> return (Some "$") in *)
  let prefix =
    choice ~failure_msg:"unrecognized prefix" [ remove_ZN; remove__ZN; removeZN ]
  in
  prefix *> many1 first_parser <* char 'E' *> end_of_input
;;

let decode_parser =
  let open Angstrom in
  let open Angstrom.Let_syntax in
  let unescape_characters =
    List.map Constants.escape_sequences ~f:(fun (escape_sequence, new_char) ->
      string escape_sequence *> return (Some new_char))
  in
  let period_to_dash = [ (string "." >>| fun _ -> Some "-") ] in
  let remove_first_underscore =
    peek_string 2
    >>= fun first_two_characters ->
    match first_two_characters with
    | None -> fail "No _$ found"
    | Some starter_string -> if String.equal starter_string "_$" then return (Some "$")
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
  let strip_h_suffix =
    string "h" *> skip_while Char.is_hex_digit *> end_of_input *> return None
  in
  let token =
    choice
      ~failure_msg:"unrecognized token"
      (unescape_characters
      @ remove_first_underscore
      @ two_digit_hexadecimal_number
      @ periods_to_semicolon
      @ period_to_dash
      @ normal_character)
  in
  let tokens =
    many1 token
    >>| fun tokens -> tokens |> List.filter_map ~f:Fn.id |> String.concat |> Some
  in
  strip_h_suffix <|> tokens
;;

let demangle mangled_symbol =
  let mangled_string = Angstrom.parse_string ~consume:All splits_parser mangled_symbol in
  let split =
    match mangled_string with
    | Ok list -> Some (List.map list ~f:Fn.id)
    | Error _ -> None
  in
  print_s [%message (split : string list option)];
  match split with
  | Some symbol ->
    Some
      (List.filter_map symbol ~f:(fun str ->
         match Angstrom.parse_string ~consume:All decode_parser str with
         | Ok list -> list
         | Error _ -> None)
      |> String.concat ~sep:"::")
  | None -> None
;;

let demangle_symbol_test symbol =
  let demangle_symbol = demangle symbol in
  print_s [%sexp (demangle_symbol : string option)]
;;

let%expect_test "real mangled symbol" =
  demangle_symbol_test
    "__ZN38_$LT$core..option..Option$LT$T$GT$$GT$6unwrap18_MSG_FILE_LINE_COL17haf7cb8d5824ee659E";
  [%expect
    {|
    38
    6
    18
    17
    (split
     ((_$LT$core..option..Option$LT$T$GT$$GT$ unwrap _MSG_FILE_LINE_COL
       haf7cb8d5824ee659)))
    (LT$core::option::Option<T>>::unwrap::_MSG_FILE_LINE_COL) |}]
;;
