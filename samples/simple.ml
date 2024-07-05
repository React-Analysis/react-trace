let C x =
  stt s, setS = if --42 = +42 then +0+42*1 else 42 in
  eff (setS (fun s -> -43));
  view [()]
;;
view [C ()]
