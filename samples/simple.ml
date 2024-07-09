let C x =
  stt s, setS = 42 in
  eff (setS (fun s -> 0));
  view [()]
;;
let D x =
  stt s, setS = true in
  eff (setS (fun s -> false));
  if s then
    view [C ()]
  else
    view [C (), C ()]
;;
view [D ()]
