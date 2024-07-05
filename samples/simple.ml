let C x =
  stt s, setS = 42 in
  eff (setS (fun s -> -43));
  view [()]
;;
view [C ()]
