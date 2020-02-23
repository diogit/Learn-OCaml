let proof_of_bug =
  [|
    {code=0; contact={name="test1";phone_number=(1,1,1,1)}};
    {code=0; contact={name="test2";phone_number=(2,2,2,2)}};
    {code=1; contact={name="test1";phone_number=(1,1,1,1)}};
    {code=0; contact={name="test1";phone_number=(1,1,1,1)}};
    {code=2; contact={name="test2";phone_number=(2,2,2,2)}}
  |] ;; 

let delete db contact = 
  let (status, db, contact) = search db contact in
  if not status then (false, db, contact)
  else
    let rec getIdx idx =
      if idx >= db.number_of_contacts then
        -1
      else if db.contacts.(idx).name = contact.name then
        idx
      else
        getIdx (idx + 1)
    in
    let deleteIdx = getIdx 0 in
    let cells i = 
      if i >= deleteIdx && i + 1 < (Array.length db.contacts) then
        db.contacts.(i + 1)
      else
        db.contacts.(i)
    in
    let db' = {
      number_of_contacts = db.number_of_contacts - 1;
      contacts = Array.init (Array.length db.contacts) cells
    }
    in
    (true, db', contact);;

let update db contact =
  match delete db contact with
  | (_, db, _) -> insert db contact
;;

let engine db { code ; contact } =
  if code = 0 then insert db contact
  else if code = 1 then delete db contact
  else if code = 2 then search db contact
  else if code = 3 then update db contact
  else (false, db, nobody);;
