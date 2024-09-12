let Item isPacked =
  if isPacked then
    view [1]
  else
    view [0]
;;
let PackingList _ =
  view [Item true, Item true, Item false]
;;
view [PackingList ()]
