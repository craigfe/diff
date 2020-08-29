open Diff
open Sink

let levenshtein (t_expected, t_actual) =
  let script = levenshtein_script List ~equal:Int.equal t_expected t_actual in
  let resulting =
    Edit_script.(apply List)
      script
      ~actual:(Array.get (Array.of_list t_actual))
      t_expected
  in
  Crowbar.check_eq ~pp:Fmt.(Dump.list int) resulting t_actual

let () =
  Crowbar.add_test ~name:"levenshtein"
    [ Crowbar.(pair (list int) (list int)) ]
    levenshtein
