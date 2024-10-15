let permutation tableau = (* Fisher-Yater-Knuth*) 
  let n = Array.length tableau in 
  let result = Array.init n (fun i -> tableau.(i)) in
  
  for i = 0 to n-2 do 
    let indice = Random.int(n-1-i) in
    let temp = result.(n-1-i) in
    result.(n-1-i) <- result.(indice);
    result.(indice) <- temp;
  done;
  
  if result.(0) = tableau.(0) then 
    begin
      let temp = result.(0) in
      result.(0) <- result.(1);
      result.(1) <- temp;
    end;
  result;;
          
let secretsanta_basique participants = 
  let nb_participants = Array.length participants in
  if nb_participants < 2 then failwith "Pas assez de participants";
  let melange = permutation participants in
  let resultat = Array.make nb_participants (participants.(0), participants.(0)) in
  for i = 0 to nb_participants -1 do
    resultat.(i) <- (participants.(i), melange.(i));
  done;
  resultat;;

let participants = [| "Alice"; "Bob"; "Charlie"; "David" |];;
secretsanta_basique participants;;
