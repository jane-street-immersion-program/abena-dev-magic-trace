open! Core

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

module Parser = struct
  open Angstrom
  open Angstrom.Let_syntax

  (* CR hlian: Try to share this code between the two demangling modules *)
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
    let check_for_hex_or_at t = Char.is_hex_digit t || Char.( = ) t '@' in
    let normal_character =
      any_char >>| fun character -> Some (Char.to_string character)
    in
    let llvm_suffix =
      normal_character
      <* string ".llvm." *> skip_while check_for_hex_or_at *> end_of_input
    in
    many1 (llvm_suffix <|> normal_character)
  ;;

  (* CR hlian: [rust_hash], maybe? Parsers aren't predicates, so it's good to avoid the
     "is_" prefix with them *)
  let is_rust_hash =
    let open Angstrom in
    string "h" *> skip_while Char.is_hex_digit *> at_end_of_input
  ;;

  (* CR hlian: "get" is a bad verb to use when naming functions. I would just drop it. A
   better name for this parser would be [split_by_length_prefixes], maybe? *)
  let get_integers_from_symbol_parser =
    let open Angstrom in
    let open Angstrom.Let_syntax in
    let int_of_string = take_while1 Char.is_digit in
    let%bind string_to_int = int_of_string in
    let integer_in_symbol = Int.of_string string_to_int in
    take integer_in_symbol
  ;;

  (* CR hlian: For parsers/functions that return tuples, generally you want to use an "and"
   in the name -- this, for example, might be better named as [splits_and_suffix]? *)
  let splits_symbol_parser =
    let open Angstrom in
    let open Angstrom.Let_syntax in
    let prefix =
      (* CR hlian: There should be a way to write this like this:
    {v
       [ "_ZN"; "__ZN"; "ZN" ] |> List.map ~f:... |> choice ~failure_msg:"unrecognized prefix"
    v}

      You can then move the list into the [Constants] module, maybe *)
      let remove_ZN = string "_ZN" *> return None in
      let remove__ZN = string "__ZN" *> return None in
      let removeZN = string "ZN" *> return None in
      (* CR hlian: Possibly [splits_and_suffix] as well? *)
      choice
        ~failure_msg:"splits_symbol_parser: unrecognized prefix"
        [ remove_ZN; remove__ZN; removeZN ]
    in
    let%bind first_half = prefix *> many1 get_integers_from_symbol_parser <* char 'E' in
    let%bind second_half = take_while (fun _ -> true) in
    return (first_half, second_half)
  ;;

  (* CR hlian: [decode_split], maybe? [parser] isn't a great name here because all these
   variables are parsers, except for the two run functions below *)
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
        | false -> fail "no _$ found"
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
      choice (* CR hlian: Possibly rename to [decode_split] here as well? *)
        ~failure_msg:"decode_parser: unrecognized token"
        (remove_first_underscore
        @ unescape_characters
        @ two_digit_hexadecimal_number
        @ periods_to_semicolon
        @ normal_character)
    in
    let%map tokens = many1 token in
    tokens |> List.filter_map ~f:Fn.id |> String.concat |> Some
  ;;
end

type t =
  { body : string
  ; suffix : string
  }

let remove_llvm_suffix mangled_symbol =
  let remove_llvm_parser =
    Angstrom.parse_string ~consume:All Parser.strip_llvm_parser mangled_symbol
  in
  let new_mangled_symbol =
    match remove_llvm_parser with
    | Ok symbol -> List.filter_map symbol ~f:Fn.id
    | Error reason -> [ reason ]
  in
  String.concat new_mangled_symbol
;;

(* CR hlian: A comment about strategy would be nice here, I think. Something like: "Rust
   symbols have to be demangled with not one but three separate parsers. Here's what each
   one does..." *)
let demangle mangled_symbol =
  let open Result.Let_syntax in
  let new_mangled_symbol = remove_llvm_suffix mangled_symbol in
  let%bind split_strings, suffix =
    Angstrom.parse_string ~consume:All Parser.splits_symbol_parser new_mangled_symbol
  in
  let filter_hash =
    List.filteri split_strings ~f:(fun index element ->
      let is_last_split = index + 1 = List.length split_strings in
      let is_split_a_hash =
        match Angstrom.parse_string ~consume:All Parser.is_rust_hash element with
        | Ok symbol -> symbol
        | Error _ -> false
      in
      not (is_last_split && is_split_a_hash))
  in
  let body =
    List.filter_map filter_hash ~f:(fun str ->
      match Angstrom.parse_string ~consume:All Parser.decode_parser str with
      | Ok symbol -> symbol
      | Error _ -> None)
    |> String.concat ~sep:"::"
  in
  return { body; suffix }
;;
(* CR hlian: don't forget to write a good commit message :^) *)
