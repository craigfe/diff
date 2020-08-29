let test_edit_script = Alcotest.(list (of_pp Diff.pp_command))

module Levenshtein = struct
  (* Basic test case *)
  let lev ?(speed = `Quick) a b msg expected =
    Alcotest.test_case msg speed (fun () ->
        let script = Diff.(levenshtein_script Array) ~equal:Int.equal a b in
        Alcotest.check test_edit_script msg expected script;
        let resulting =
          Diff.(Edit_script.apply Array) ~actual:(Array.get b) script a
          |> Array.of_list
        in
        Alcotest.(check (array int))
          "Applying the script to [expected] should recover [actual]" b
          resulting)

  let long_arrays =
    let size = 5_000 in
    let long_array1 = Array.init size Fun.id in
    let long_array2 = Array.init size (( + ) size) in
    let expected =
      List.init size (fun i -> Diff.Substitute { expected = i; actual = i })
    in
    lev ~speed:`Slow long_array1 long_array2
      (Fmt.str "Non-equal arrays with %d elements" size)
      expected

  let suite =
    let open Diff in
    let i expected actual = Insert { expected; actual }
    and d expected = Delete { expected }
    and s expected actual = Substitute { expected; actual } in

    [
      lev [||] [||] "Empty" [];
      lev [| 1 |] [| 1 |] "Noop" [];
      lev [| 1 |] [||] "Delete 1" [ d 0 ];
      lev [| 1; 2 |] [||] "Delete 2" [ d 0; d 1 ];
      lev [| 1; 2; 3 |] [||] "Delete 3" [ d 0; d 1; d 2 ];
      lev [| 1; -1; 2; 3 |] [| -1 |] "Delete disjoint" [ d 0; d 2; d 3 ];
      lev [||] [| 1 |] "Insert 1" [ i 0 0 ];
      lev [||] [| 1; 2 |] "Insert 2" [ i 0 0; i 0 1 ];
      lev [||] [| 1; 2; 3 |] "Insert 3" [ i 0 0; i 0 1; i 0 2 ];
      lev [| 1 |] [| 2; 3; 1; 4 |] "Insert disjoint" [ i 0 0; i 0 1; i 1 3 ];
      lev [| 1 |] [| 2 |] "Single substitute (1/1)" [ s 0 0 ];
      lev [| 10; 2 |] [| 11; 2 |] "Single substitute (1/2)" [ s 0 0 ];
      lev [| 2; 10 |] [| 2; 11 |] "Single substitute (2/2)" [ s 1 1 ];
      lev [| 1; 0; 3 |] [| 1; 2; 3 |] "Single substitute (2/3)" [ s 1 1 ];
      lev
        [| 1; 2; 3; -4; 5; 6; 7; -8; 9; 10 |]
        [| 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 |]
        "Multiple substitutes" [ s 3 3; s 7 7 ];
      lev [| 1; 1 |] [| 1; 1; 2; 3; 4 |] "Common prefix" [ i 2 2; i 2 3; i 2 4 ];
      lev [| 1; 2; 1 |] [| 1 |] "Common prefix and suffix" [ d 1; d 2 ];
      lev [| -1; 3; 4; 5 |] [| 1; 2; 3; 4; 5 |] "Substitute then insert"
        [ s 0 0; i 1 1 ];
      lev [| 1 |] [| 2; 3; 4; 5 |] "Substitute then inserts"
        [ s 0 0; i 1 1; i 1 2; i 1 3 ];
      lev [| 1; 0; 3 |] [| 1; 2 |] "Substitute then delete" [ s 1 1; d 2 ];
      long_arrays;
    ]
end

let () =
  Logs.set_level (Some Logs.Debug);
  Logs.set_reporter (Logs_fmt.reporter ());
  Alcotest.run "test_diff" [ ("levenshtein", Levenshtein.suite) ]
