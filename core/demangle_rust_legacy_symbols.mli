open! Core

(* XCR clive for hlian: good docstring!!  *)

(** The logic for this function is derived from the [rustc-demangle] crate [0]. This
    function is used to change the symbols in the application executable from a mangled
    form to a demangled form in Rust. Now when running [magic-trace run -trigger] the
    symbols will appear in their demangled form. Will return [None] if the symbol is not
    recognized as an Rust symbol.

    [0]: https://github.com/rust-lang/rustc-demangle/blob/main/src/lib.rs#L130 *)
type t =
  { body : string (* CR hlian: document this field *)
  ; suffix : string (* CR hlian: document this field *)
  }

val demangle : string -> (t, string) result
