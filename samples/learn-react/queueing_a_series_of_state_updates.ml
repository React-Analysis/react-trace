let Counter _ =
  let (number, setNumber) = useState 0 in
  if number = 0 then (
    setNumber (fun _ -> number + 5);
    setNumber (fun n -> n + 1);
    setNumber (fun _ -> 42)
  );
  view [number]
;;
view [Counter ()]
