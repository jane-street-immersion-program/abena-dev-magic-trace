open! Core
open Magic_trace_core

let demangle_symbol_test (input, expected) =
  let demangle_symbol =
    Demangle_rust_symbols.demangle_symbol input
    |> Result.map_error ~f:(String.( ^ ) input)
    |> Result.ok_or_failwith
  in
  match String.( = ) demangle_symbol expected with
  | true -> ()
  | false ->
    print_s
      [%message
        "unexpectedly got"
          (demangle_symbol : string)
          "when we were expecting"
          (expected : string)]
;;

let demangle_symbol_test_error input =
  let demangle_symbol = Demangle_rust_symbols.demangle_symbol input in
  match demangle_symbol with
  | Error _ -> ()
  | Ok symbol -> print_string symbol
;;

let%expect_test "demangle" =
  demangle_symbol_test_error "test";
  demangle_symbol_test ("_ZN4testE", "test");
  demangle_symbol_test_error "_ZN4test";
  demangle_symbol_test ("_ZN4test1a2bcE", "test::a::bc");
  [%expect {| |}]
;;

let%expect_test "demangle_dollars" =
  demangle_symbol_test ("_ZN4$RP$E", ")");
  demangle_symbol_test ("_ZN8$RF$testE", "&test");
  demangle_symbol_test ("_ZN8$BP$test4foobE", "*test::foob");
  demangle_symbol_test ("_ZN9$u20$test4foobE", " test::foob");
  demangle_symbol_test ("_ZN35Bar$LT$$u5b$u32$u3b$$u20$4$u5d$$GT$E", "Bar<[u32; 4]>");
  [%expect {| |}]
;;

let%expect_test "demangle_many_dollars" =
  demangle_symbol_test ("_ZN13test$u20$test4foobE", "test test::foob");
  demangle_symbol_test ("_ZN12test$BP$test4foobE", "test*test::foob");
  [%expect {| |}]
;;

let%expect_test "demangle_windows" =
  demangle_symbol_test ("ZN4testE", "test");
  demangle_symbol_test ("ZN13test$u20$test4foobE", "test test::foob");
  demangle_symbol_test ("ZN12test$RF$test4foobE", "test&test::foob");
  [%expect {| |}]
;;

let%expect_test "demangle_elements_beginning_with_underscore" =
  demangle_symbol_test ("_ZN13_$LT$test$GT$E", "<test>");
  demangle_symbol_test ("_ZN28_$u7b$$u7b$closure$u7d$$u7d$E", "{{closure}}");
  demangle_symbol_test ("_ZN15__STATIC_FMTSTRE", "__STATIC_FMTSTR");
  [%expect {| |}]
;;

let%expect_test "demangle_trait_impls" =
  demangle_symbol_test
    ( "_ZN71_$LT$Test$u20$$u2b$$u20$$u27$static$u20$as$u20$foo..Bar$LT$Test$GT$$GT$3barE"
    , "<Test + 'static as foo::Bar<Test>>::bar" );
  [%expect {| |}]
;;

let%expect_test "demangle_without_hash" =
  let s = "_ZN3foo17h05af221e174051e9E" in
  demangle_symbol_test (s, "foo");
  [%expect {| |}]
;;

let%expect_test "demangle_without_hash_edgecases" =
  (* One element, no hash. *)
  demangle_symbol_test ("_ZN3fooE", "foo");
  (* Two elements, no hash. *)
  demangle_symbol_test ("_ZN3foo3barE", "foo::bar");
  (* Longer-than-normal hash. *)
  demangle_symbol_test ("_ZN3foo20h05af221e174051e9abcE", "foo");
  (* Shorter-than-normal hash. *)
  demangle_symbol_test ("_ZN3foo5h05afE", "foo");
  (* Valid hash, but not at the end. *)
  demangle_symbol_test ("_ZN17h05af221e174051e93fooE", "h05af221e174051e9::foo");
  (* Not a valid hash, missing the 'h'. *)
  demangle_symbol_test ("_ZN3foo16ffaf221e174051e9E", "foo::ffaf221e174051e9");
  (* Not a valid hash, has a non-hex-digit *)
  demangle_symbol_test ("_ZN3foo17hg5af221e174051e9E", "foo::hg5af221e174051e9");
  [%expect {| |}]
;;

let%expect_test "demangle_thinlto" =
  (* One element, no hash. *)
  demangle_symbol_test ("_ZN3fooE.llvm.9D1C9369", "foo");
  demangle_symbol_test ("_ZN3fooE.llvm.9D1C9369@@16", "foo");
  demangle_symbol_test
    ("_ZN9backtrace3foo17hbb467fcdaea5d79bE.llvm.A5310EB9", "backtrace::foo");
  [%expect {| |}]
;;

let%expect_test "demangle_llvm_ir_branch_labels" =
  demangle_symbol_test
    ( "_ZN4core5slice77_$LT$impl$u20$core..ops..index..IndexMut$LT$I$GT$$u20$for$u20$$u5b$T$u5d$$GT$9index_mut17haf9727c2edfbc47bE.exit.i.i"
    , "core::slice::<impl core::ops::index::IndexMut<I> for [T]>::index_mut.exit.i.i" );
  [%expect {| |}]
;;

let%expect_test "demangle_ignores_suffix_that_doesnt_look_like_a_symbol" =
  demangle_symbol_test_error "_ZN3fooE.llvm moocow";
  [%expect {|
    foo |}]
;;

let%expect_test "invalid_no_chop" =
  demangle_symbol_test_error "_ZNfooE";
  [%expect {| |}]
;;

let%expect_test "handle_assoc_types" =
  demangle_symbol_test
    ( "_ZN151_$LT$alloc..boxed..Box$LT$alloc..boxed..FnBox$LT$A$C$$u20$Output$u3d$R$GT$$u20$$u2b$$u20$$u27$a$GT$$u20$as$u20$core..ops..function..FnOnce$LT$A$GT$$GT$9call_once17h69e8f44b3723e1caE"
    , "<alloc::boxed::Box<alloc::boxed::FnBox<A, Output=R> + 'a> as \
       core::ops::function::FnOnce<A>>::call_once" );
  [%expect {| |}]
;;

let%expect_test "handle_bang" =
  demangle_symbol_test
    ( "_ZN88_$LT$core..result..Result$LT$$u21$$C$$u20$E$GT$$u20$as$u20$std..process..Termination$GT$6report17hfc41d0da4a40b3e8E"
    , "<core::result::Result<!, E> as std::process::Termination>::report" );
  [%expect {| |}]
;;

(* This case was not implemented. Useful for future iterations of demangling rust symbols*)
let%expect_test "demangle_utf8_idents" =
  demangle_symbol_test
    ( "_ZN11utf8_idents157_$u10e1$$u10d0$$u10ed$$u10db$$u10d4$$u10da$$u10d0$$u10d3$_$u10d2$$u10d4$$u10db$$u10e0$$u10d8$$u10d4$$u10da$$u10d8$_$u10e1$$u10d0$$u10d3$$u10d8$$u10da$$u10d8$17h21634fd5714000aaE"
    , "utf8_idents::საჭმელად_გემრიელი_სადილი" );
  [%expect
    {|
    ("unexpectedly got"
     (demangle_symbol
      utf8_idents::$u10e1$$u10d0$$u10ed$$u10db$$u10d4$$u10da$$u10d0$$u10d3$$u10d2$$u10d4$$u10db$$u10e0$$u10d8$$u10d4$$u10da$$u10d8$$u10e1$$u10d0$$u10d3$$u10d8$$u10da$$u10d8$)
     "when we were expecting"
     (expected
      "utf8_idents::\225\131\161\225\131\144\225\131\173\225\131\155\225\131\148\225\131\154\225\131\144\225\131\147_\225\131\146\225\131\148\225\131\155\225\131\160\225\131\152\225\131\148\225\131\154\225\131\152_\225\131\161\225\131\144\225\131\147\225\131\152\225\131\154\225\131\152")) |}]
;;

let%expect_test "demangle_issue_60925" =
  demangle_symbol_test
    ( "_ZN11issue_609253foo37Foo$LT$issue_60925..llv$u6d$..Foo$GT$3foo17h059a991a004536adE"
    , "issue_60925::foo::Foo<issue_60925::llvm::Foo>::foo" );
  [%expect {| |}]
;;
