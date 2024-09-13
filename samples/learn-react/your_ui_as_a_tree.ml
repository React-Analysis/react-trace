let FancyText x =
  if x = 0 then
    view [0]
  else
    view [1]
;;
let InspirationGenerator children =
  let (index, setIndex) = useState 0 in
  # need to have indexing
  children
;;
let Copyright year =
  view [year]
;;
let App _ =
  view [FancyText 42, InspirationGenerator (view [Copyright 2004])]
;;
view [App ()]
