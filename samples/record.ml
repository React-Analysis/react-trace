let C x =
  stt s, setS = (let r = {} in r.x := 42; r) in
  eff (if s.x <= 45 then setS (fun s -> (let r = {} in r.x := s.x + 1; r)));
  view [()]
;;
view [C ()]
