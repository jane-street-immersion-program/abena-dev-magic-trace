open! Core

(* CR hlian: This docstring is duplicated with the other one. This one you should probably
   keep (it's very good!), and the other one you should replace with documentation about
   what "legacy" means in this context *)

(** The logic for this function is derived from the [rustc-demangle] crate [0]. This
    function is used to change the symbols in the application executable from a mangled
    form to a demangled form in Rust. Now when running [magic-trace run -trigger] the
    symbols will appear in their demangled form. Will return [None] if the symbol is not
    recognized as an Rust symbol.

    [0]: https://github.com/rust-lang/rustc-demangle/blob/main/src/lib.rs#L130 *)
val demangle_symbol : string -> (string, string) Result.t

(* CR hlian: You should document why this returns a tuple *)
