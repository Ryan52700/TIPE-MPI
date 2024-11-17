
let permutation tableau = (* Fisher-Yater-Knuth, mélange de manière aléatoire un tableau *) 
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

let secretsanta_basique participants = (* Renvoie sous forme de tableau de couple les couples *)
  let nb_participants = Array.length participants in
  if nb_participants < 2 then failwith "Pas assez de participants";
  let melange = permutation participants in
  let resultat = Array.make nb_participants (participants.(0), participants.(0)) in
  for i = 0 to nb_participants -1 do
    resultat.(i) <- (participants.(i), melange.(i));
  done;
  resultat;;

let participants = [| "Alice"; "Bob"; "Charlie"; "David" ; "Ryan"; "Valentin"; "Yani"; "Clément"; "Gabriel"; "Marouane"; "Le Patron";
                      "La Patronne" |];;
secretsanta_basique participants;;


let groupe_cyclique p q = (* Renvoie (generateur,groupe) *)
  let groupe = Array.make q 0 in
  groupe.(0) <- 1;
  let boucle = ref true in
  let i = ref 2 in
  while( !i < q && !boucle) do 
    if ( (int_of_float (float_of_int(!i) ** float_of_int(q)) mod p) = 1) then
      begin 
        boucle := false;
        for k = 1 to (q-1) do
          groupe.(k) <- (groupe.(k-1) * !i) mod p;
        done;
      end
    else ();
    i := !i + 1; 
  done;
  
  if groupe.(1) <> 0 then 
    begin
      i := !i - 1;
      (!i, groupe);
    end
    
  else 
    begin
      for k = 0 to (q-1) do
        groupe.(k) <- 1;
      done;
      i := 1;
      (!i, groupe);
    end;; 

let generateur_cle participants = 
  
  let q = 7919 in
  let p = 7561 in 
  let result = Hashtbl.create 8 in
  let groupe = snd (groupe_cyclique p q) in
  
  for i = 0 to (Array.length participants - 1) do
    Hashtbl.add result participants.(i) groupe.(Random.int(q-1));
  done;
  result;;
  
let association participants = (* Renvoie un dictionnaire qui a associé à chaque participants un nombre *)
  
  let random = permutation participants in
  let association1 = Hashtbl.create 8 in
  let association2 = Hashtbl.create 8 in
  
  for i = 0 to (Array.length participants - 1) do
    Hashtbl.add association1 random.(i) i;
    Hashtbl.add association2 i random.(i);
  done;

  (association1, association2);;

let secretsanta_chiffre_un (participants: string array) cles = (* Même fonctionnement que le secretsanta_basique mais le deuxième membre
                                             du couple est codé. Prends en second arguments les clés des participants *)
  
  let temp = secretsanta_basique participants in
  let (nom_nombre, nombre_nom) = association participants in
  let q = 11 in
  let p = 5 in 
  
  let (generateur, groupe) = groupe_cyclique p q in
  let k = groupe.(Random.int(q-1)) in
  let publique = Hashtbl.create 8 in
  
  for i = 0 to (Array.length participants - 1)do
    Hashtbl.add publique participants.(i) (int_of_float(float_of_int(generateur) ** float_of_int(Hashtbl.find cles participants.(i))) mod p);
  done; 
  let result = Array.make (Array.length participants) (participants.(0), (0,0) ) in
  
  for i = 0 to (Array.length participants - 1) do
    result.(i) <- (participants.(i), (int_of_float(float_of_int(generateur) ** float_of_int(k)) mod p, 
                                      int_of_float(float_of_int(Hashtbl.find publique participants.(i)) ** float_of_int(k)) * (Hashtbl.find nom_nombre (snd temp.(i))) mod p));
  done;
  
  
  (result, nom_nombre, nombre_nom, publique);; (* Result de la forme (participants, (u,v) ), accosiations est un Hashtbl avec comme clés les noms des participants
                                        et publique est un Hashtbl avec comme clé les noms des participants*)
  
let participants = [| "Alice"; "Bob"; "Charlie"; "David" ; "Ryan"; "Valentin"; "Yani"; "Clément"; "Gabriel"; "Marouane"; "Le Patron";
                      "La Patronne" ; " Test" |];; 
let cles_prives = generateur_cle participants;; 
secretsanta_chiffre_un participants cles_prives;;




