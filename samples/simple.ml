let C x =
  let (s, setS) = useState 42 in
  useEffect (setS (fun s -> 0));
  view [()]
;;
view [C ()]
