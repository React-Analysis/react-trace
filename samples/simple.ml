let C x =
  stt s, setS = 42 in
  eff (setS (fun s -> 0));
  view [()]
;;
view [C ()]
