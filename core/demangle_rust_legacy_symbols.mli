open! Core

(** The logic for this function is derived from rust-lang [0].
    This function is used to change the symbols in the application
    executable from a mangled form to a demangled form in Rust. Now
    when running [magic-trace run -trigger] the symbols will appear
    in their demangled form. Will return None if the symbol is not
    recognized as an Rust symbol. 
    
    [0]: https://github.com/rust-lang/rustc-demangle/blob/main/src/lib.rs#L130 *)
type t =
  { body : string
  ; suffix : string
  }

val demangle : string -> (t, string) result
