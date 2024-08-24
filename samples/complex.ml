let C x =
  stt s, setS = x in
  if s = 42 then
    setS (fun s -> s + 1);
  view [()]
;;
let D _ =
  stt s, setS = true in
  eff (setS (fun _ -> false));
  view [C 42]
;;
view [D (), 0]
