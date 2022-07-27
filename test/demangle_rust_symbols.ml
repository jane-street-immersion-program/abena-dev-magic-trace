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
    ; "$u20$", " "
    ; "$u27$", "\'"
    ; "$u5b$", "["
    ; "$u5d$", "]"
    ; "$u7e$", "~"
    ]
  ;;
end

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

let many_parser =
  let open Angstrom in
  let remove_ZN = string "_ZN" *> return None in
  let remove__ZN = string "__ZN" *> return None in
  let removeZN = string "ZN" *> return None in
  let prefix =
    choice ~failure_msg:"unrecognized token" [ remove_ZN; remove__ZN; removeZN ]
  in
  prefix *> many1 first_parser <* char 'E' *> end_of_input
;;

(* let parser =
  let open Angstrom in
  let strip_h_suffix =
    [ string "h" *> skip_while Char.is_hex_digit *> end_of_input *> return "" ]
  in
  let _unescape_characters =
    List.map Constants.escape_sequences ~f:(fun (escape_sequence, new_char) ->
      string escape_sequence *> return new_char)
  in
  let _period_to_dash = [ (string "." >>| fun _ -> "-") ] in
  let _periods_to_semicolon = [ (string ".." >>| fun _ -> "::") ] in
  let normal_character = [ (any_char >>| fun character -> Char.to_string character) ] in
  let token =
    choice
      ~failure_msg:"unrecognized token"
      (strip_h_suffix
      @ unescape_characters
      @ periods_to_semicolon
      @ period_to_dash
      @ normal_character)
  in
  many1 token
;; *)

let demangle mangled_symbol =
  let mangled_string = Angstrom.parse_string ~consume:All many_parser mangled_symbol in
  print_s [%sexp (mangled_string : (string list, string) Result.t)]
;;

(* match mangled_string with
  | Ok list -> Some (String.concat (List.map list ~f:Fn.id))
  | Error _ -> None *)

let demangle_symbol_test symbol =
  let demangle_symbol = demangle symbol in
  print_s [%sexp (demangle_symbol : unit)]
;;

let%expect_test "real mangled symbol" =
  demangle_symbol_test
    "Zn4core3fmt3num50_$LT$impl$u20$core..fmt..Debug$u20$for$u20$i32$GT$3fmt17hc0f8e4abd2ee612cE";
  [%expect {|
    (Error ": unrecognized token")
    () |}]
;;
