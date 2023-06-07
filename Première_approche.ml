#require "graphics"
#load "graphics.cma"


type lieu = int
type route = (lieu*lieu*float*int)
(* départ, arrivé, a = temps de plus par voiture, b = temps initial *)
type trajet = lieu list
type rep_route = route list
type mat_graphe = (float*int) option array array

(*

let r_route : rep_route = [(0,1,0.,40);(2,3,0.,40);(0,2,1.,0);(1,3,1.,0);(2,1,0.,0);(1,2,0.,0)]
let nb_lieu = 4
let dep = 0
let arr = 3
let nb_voiture = 250

let fonction_temps_route (a:float) (b:int) (x:int) =
    (float_of_int(b)+.a*.float_of_int(x))
*)

let vp = 450.						(* petite route non encombrée - tps de + par voit *)
let vm = 300.						(* petite route moyennement encombrée - tps de + par voit *)
let vg = 150.						(* petite route très encombrée - tps de + par voit *)
let op = 1000.						(* petite route non encombrée - tps de + par voit *)
let om = 850.						(* petite route moyennement encombrée - tps de + par voit *)
let og = 700.						(* petite route très encombrée - tps de + par voit *)
let rp = 1550.						(* petite route non encombrée - tps de + par voit *)
let rm = 1400.						(* petite route moyennement encombrée - tps de + par voit *)
let rg = 1250.						(* petite route très encombrée - tps de + par voit *)

let nb_voiture = 1750

let r_route : rep_route = [
(1,0,og,200);(13,0,vg,400);(14,0,og,43);
(0,1,vg,212);(21,1,vg,166);
(3,2,rg,133);(20,2,og,43);(21,2,rg,100);
(2,3,og,133);(4,3,og,66);(35,3,om,83);
(3,4,og,63);(5,4,og,33);
(6,5,og,40);
(4,6,og,166);(7,6,og,133);
(6,7,og,133);(8,7,og,217);
(7,8,og,217);(9,8,og,150);(33,8,op,450);(42,8,om,175);
(8,9,og,133);(10,9,og,217);
(9,10,og,233);(11,10,rg,80);(43,10,op,550);
(10,11,vg,77);(30,11,om,150);(36,11,og,37);
(13,12,vg,166);(31,12,vg,133);(36,12,og,66);(45,12,om,150);
(0,13,og,366);(12,13,og,266);(31,13,om,105);
(0,14,og,33);(15,14,og,117);(16,14,vg,133);
(1,15,vp,700);(23,15,og,117);
(14,16,og,133);(15,16,op,300);(25,16,vg,100);
(18,17,og,117);(40,17,rp,170);
(19,18,og,93);(37,18,og,66);(41,18,rm,45);
(20,19,op,450);(35,19,og,60);(48,19,op,350);
(2,20,om,70);(48,20,vp,350);
(1,21,rg,183);(2,21,og,100);(22,21,om,125);
(2,22,og,117);(24,22,vm,250);
(17,23,og,17);(46,23,rp,350);(24,23,vp,60);
(22,24,om,300);(23,24,vp,60);
(16,25,vm,150);(46,25,op,350);(47,25,op,500);
(25,26,om,225);(27,26,om,135);
(25,27,vm,175);(30,27,om,140);
(27,28,om,70);(29,28,om,175);(38,28,vp,270);
(26,29,om,55);
(28,30,vm,140);(38,30,vm,100);(44,30,vm,95);
(13,31,vm,90);(32,31,vm,175);(45,31,op,250);
(25,32,op,270);(44,32,vm,175);
(41,33,om,115);
(5,34,op,390);(35,34,op,270);
(3,35,om,125);(19,35,og,60);(20,35,vm,175);
(11,36,vg,40);(12,36,og,66);
(7,37,og,100);(34,37,op,300);
(10,38,op,240);(43,38,op,400);
(26,39,op,280);
(39,40,rp,270);
(18,41,rp,100);(33,41,om,90);
(9,42,op,550);(29,42,rp,170);
(9,43,vp,300);
(30,44,om,90);(36,44,vm,95);
(32,45,vp,250);
(39,46,op,300);
(31,47,vm,150);
(17,48,om,150)]
let nb_lieu = 49
let dep = 34
let arr = 31 (*25 cool*) (*39 montagne*)


let fonction_temps_route (a:float) (b:int) (x:int) =
    (float_of_int(b)/.30.)*.(1.+.0.2*.(float_of_int(x)/.a)**10.)



let graphe_trajet (cr:rep_route) : mat_graphe =
    let tab = Array.make_matrix nb_lieu nb_lieu None in
    let rec aux (crr:rep_route) : mat_graphe =
        match crr with
        |(dep,arr,a,b)::l ->
            tab.(dep).(arr) <- Some(a,b);
            aux l
        |[] -> tab
    in aux cr
    
let graphe_trajet_sans_i (cr:rep_route) (i:int) : mat_graphe = 
    let f x = x<>List.nth cr i in
    graphe_trajet (List.filter f cr)


    
let liste_trajet (m:mat_graphe) : trajet list =
    let rec aux (d:lieu)(t:trajet)(tl:trajet list)(i:int): trajet list =
    if i = nb_lieu then tl else
        match m.(d).(i) with
        |None -> aux d t tl (i+1)
        |Some (v,c) ->
            if i=arr then aux d t ((List.rev(arr::(d::t)))::tl) (i+1)
            else if (List.mem i t) then (aux d t tl (i+1))
            else (aux i (d::t) tl 0)@(aux d t tl (i+1))
    in aux dep [] [] 0
    
let rem_liste (lt:trajet list) : trajet list =
    let rec aux (l1:trajet list) (l2:trajet list): trajet list =
        match l1 with
             |[]->l2
            |e::l ->
                if (List.mem e l2) then aux (List.tl l1) l2
                else aux (List.tl l1) (e::l2)
    in aux lt []




let min_list (nb_lieu_max:int) (lt:trajet list) : trajet list =
    List.filter (fun x -> (List.length x) < nb_lieu_max) lt    


let mat_temps_route (g:mat_graphe) : (int->float) option array array =
    let tab = Array.make_matrix nb_lieu nb_lieu None in
    for i = 0 to nb_lieu-1 do
        for j = 0 to nb_lieu-1 do
            match g.(i).(j) with
            |None -> tab.(i).(j) <- None
            |Some(a,b) ->    tab.(i).(j)<-Some(fonction_temps_route a b)
        done;
    done;
    tab
    



let rep_ego (mtr:(int->float) option array array)(lt:trajet list)(nb:int) : int option array array =
    let rep : int option array array = Array.make_matrix nb_lieu nb_lieu None in
    for i = 0 to nb_lieu-1 do
        for j = 0 to nb_lieu-1 do
            match mtr.(i).(j) with
            |None -> rep.(i).(j) <- None
            |Some(_) -> rep.(i).(j) <- Some(0)
        done;
    done;
    for k = 0 to nb-1 do (* Pour toutes les voitures *)
        let traj_min = ref [] in
        let min_t = ref (10.**16.) in
        for j = 0 to List.length lt-1 do (* Pour tous les trajets *)
            let t = List.nth lt j in
            let som_t = ref 0. in
            let lieu_1 = ref dep in
            for k = 1 to (List.length t)-1 do (* Pour les routes du trajet*)
                let lieu_2 = List.nth t k in
                match rep.(!lieu_1).(lieu_2),mtr.(!lieu_1).(lieu_2) with
                |Some(nb_v),Some(f_tps) ->
                	let tps = f_tps (nb_v+1) in
                	som_t:=!som_t +. tps;
              		lieu_1:=lieu_2;
                |_ -> failwith "erreur"
            done;
            if !som_t < !min_t then
                begin min_t:=!som_t; traj_min:=(List.nth lt j) end
            else ()
        done;
        let r_1 = ref dep in
        for i = 1 to (List.length !traj_min)-1 do
            let r_2 = List.nth !traj_min i in
            match (rep.(!r_1).(r_2)) with
            |None -> failwith "erreur"
            |Some(nb_v) ->
            	rep.(!r_1).(r_2) <- Some(nb_v + 1);
            	r_1:=r_2;   
        done;
    done;
    rep

    
let temps (rep:int option array array)(mtr:(int->float) option array array)(nb_t:int) : float =
    let somme_temps = ref 0. in
    for i = 0 to nb_lieu-1 do
        for j = 0 to nb_lieu-1 do
            if rep.(i).(j) = None
                then ()
            else
            	match mtr.(i).(j),rep.(i).(j) with
                |Some(f),Some(nb_v) ->
                	if (nb_v = 0)
                	    then ()
               		else
                   	    somme_temps:=!somme_temps +. (f nb_v)*.float_of_int(nb_v);
                |_ -> ()
        done;
    done;
    (!somme_temps)/.float_of_int(nb_t)
	
let liste_temps_rep_ego (mtr:(int->float) option array array)(lt:trajet list)(voit_max:int) =
    let tps_l = ref [] in
    let rep : int option array array = Array.make_matrix nb_lieu nb_lieu None in
    for i = 0 to nb_lieu-1 do
        for j = 0 to nb_lieu-1 do
            match mtr.(i).(j) with
            |None -> rep.(i).(j) <- None
            |Some(_) -> rep.(i).(j) <- Some(0)
        done;
    done;
    for e = 0 to voit_max-1 do (* Pour toutes les voitures *)
        print_int e; print_newline();
        let traj_min = ref [] in
        let min_t = ref (10.**16.) in
          let rec aux1 (ltt:trajet list) : unit =
          	match ltt with
          	|[] -> ()
          	|t::lttt ->
            	let som_t = ref 0. in
            	let lieu_1 = ref dep in
          		let rec aux2 (tr:trajet) : unit =
          		match tr with
          		|[] -> ()
          		|lieu_2::ttr ->
	              match rep.(!lieu_1).(lieu_2),mtr.(!lieu_1).(lieu_2) with
	              |Some(nb_v),Some(f_tps) ->
	              	let tps = f_tps (nb_v+1) in
	              	som_t:=!som_t +. tps;
	              	lieu_1:=lieu_2;
	              	aux2 ttr
	              |_ -> failwith "erreur"
	            in aux2 (List.tl t);
            if !som_t < !min_t then
                begin min_t:=!som_t; traj_min:=t end;
            aux1 lttt
          in aux1 lt;
        let r_1 = ref dep in
         let rec aux3 (traj:trajet) : unit =
		     match traj with
		      |[] -> ()
		      |r_2::suiv -> begin
		      	match (rep.(!r_1).(r_2)) with
		          |Some(nb_v) ->
		          	rep.(!r_1).(r_2) <- Some(nb_v + 1);
		          	r_1:=r_2;   
		      		|None -> failwith "erreur"
		      end;
		      aux3 suiv;
        in aux3 (List.tl !traj_min);
        tps_l:=((e+1),(temps rep mtr (e+1)))::(!tps_l)
    done;
    List.rev (!tps_l)


let rep_social (mtr:(int->float) option array array)(lt:trajet list)(nb:int) : int option array array =
(*A chaque nouvelle voiture, on la met sur tous les trajet pour voir au global quel est le temps le plus avantagueux*)
    let rep : int option array array = Array.make_matrix nb_lieu nb_lieu None in
    for i = 0 to nb_lieu-1 do
        for j = 0 to nb_lieu-1 do
            match mtr.(i).(j) with
            |None -> rep.(i).(j) <- None
            |Some(_) -> rep.(i).(j) <- Some(0)
        done;
    done;
    let rec aux (repp:int option array array) (voit_fait:int) : int option array array =
    if (voit_fait = nb) then repp
    else begin
        let min = ref (10.**16.) in
        let min_t = ref [] in
        for i = 0 to (List.length lt)-1 do (* Pour tous les trajets*)
            let t = List.nth lt i in (* On sort le trajet *)
            let lieu_1 = ref dep in
            let rep2 : int option array array = Array.make_matrix nb_lieu nb_lieu None in
            for i = 0 to nb_lieu-1 do
                for j = 0 to nb_lieu-1 do
                    match repp.(i).(j) with
                    |None -> rep2.(i).(j) <- None
                    |Some(k) -> rep2.(i).(j) <- Some(k)
                done;
            done;
            for k = 1 to (List.length t)-1 do (* Pour les routes du trajet*)
                let lieu_2 = List.nth t k in
                match rep2.(!lieu_1).(lieu_2) with
                |Some(nb_v) ->
                	rep2.(!lieu_1).(lieu_2) <- Some(nb_v+1);
               		lieu_1:=lieu_2;
               	|None -> failwith "erreur"
            done;
            let tps = temps rep2 mtr (voit_fait+1) in
            if (tps < !min)
                then begin min:=tps; min_t:=t; end
            else ()
        done;
        let lieu_1 = ref dep in
        for k = 1 to (List.length !min_t)-1 do (* Pour les routes du trajet*)
            let lieu_2 = List.nth !min_t k in
            match repp.(!lieu_1).(lieu_2) with
            |Some(nb_voi) ->
            	repp.(!lieu_1).(lieu_2) <- Some(nb_voi+1);
            	lieu_1:=lieu_2;
        		|None -> failwith "erreur"
        done;
        aux rep (voit_fait+1)
    end
    in aux rep 0


let rep_social_plus_1 (repo:int option array array)(mtr:(int->float) option array array)(lt:trajet list)(n:int) : int option array array =
(*A chaque nouvelle voiture, on la met sur tous les trajet pour voir au global quel est le temps le plus avantagueux*)
    let rep = Array.copy repo in
    let min = ref (10.**16.) in
    let min_t = ref [] in
    for i = 0 to (List.length lt)-1 do (* Pour tous les trajets*)
        let t = List.nth lt i in (* On sort le trajet *)
        let lieu_1 = ref dep in
        let rep2 : int option array array = Array.make_matrix nb_lieu nb_lieu None in
        for i = 0 to nb_lieu-1 do
            for j = 0 to nb_lieu-1 do
                match rep.(i).(j) with
                |None -> rep2.(i).(j) <- None
                |Some(k) -> rep2.(i).(j) <- Some(k)
            done;
        done;
        for k = 1 to (List.length t)-1 do (* Pour les routes du trajet*)
            let lieu_2 = List.nth t k in
            match rep2.(!lieu_1).(lieu_2) with
            |Some(nb_v) ->
            	rep2.(!lieu_1).(lieu_2) <- Some(nb_v+1);
           		lieu_1:=lieu_2;
           	|None -> failwith "erreur"
        done;
        let tps = temps rep2 mtr (n+1) in
        if (tps < !min)
            then begin min:=tps; min_t:=t; end
        else ()
    done;
    let lieu_1 = ref dep in
    for k = 1 to (List.length !min_t)-1 do (* Pour les routes du trajet*)
        let lieu_2 = List.nth !min_t k in
        match rep.(!lieu_1).(lieu_2) with
        |Some(nb_voi) ->
        	rep.(!lieu_1).(lieu_2) <- Some(nb_voi+1);
        	lieu_1:=lieu_2;
    		|None -> failwith "erreur"
    done;
    rep







(*
let liste_temps_rep_social (mtr:(int->float) option array array)(lt:trajet list)(ecart:int)(voit_max:int) : (int*float) list =
    let rep : int option array array = Array.make_matrix nb_lieu nb_lieu None in
    for i = 0 to nb_lieu-1 do
        for j = 0 to nb_lieu-1 do
            match mtr.(i).(j) with
            |None -> rep.(i).(j) <- None
            |Some(_) -> rep.(i).(j) <- Some(0)
        done;
    done;
    
    
    (*
    On extrait chaque trajet disponible
    On calcule le temps avec la voiture en plus sur le trajet extrait
    On garde le trajet où le temps total est le plus bas*)
    
    
    let rec aux (repp:int option array array) (voit_fait:int) (tps_l:(int*float) list) : (int*float) list =
    if voit_fait = voit_max then List.rev tps_l
    else begin
        let min = ref (10.**16.) in
        let min_t = ref [] in
        	let rec aux0 (ltt : trajet list) : unit =
        		match ltt with
        		|[] -> ()
        		|t::lttt ->
		          let lieu_1 = ref dep in
		          let rep2 : int option array array = Array.make_matrix nb_lieu nb_lieu None in
							for i = 0 to nb_lieu-1 do
									for j = 0 to nb_lieu-1 do
										  match rep.(i).(j) with
										  |None -> rep2.(i).(j) <- None
										  |Some(k) -> rep2.(i).(j) <- Some(k)
									done;
							done;
		          	let rec aux1 (traj:trajet) : unit =
		          		match traj with
		          		|[] -> ()
		          		|lieu_2::rest ->
				            match rep2.(!lieu_1).(lieu_2) with
				            |Some(nb_v) ->
				            	rep2.(!lieu_1).(lieu_2) <- Some(nb_v+1);
				            	lieu_1:=lieu_2;
				            |None -> failwith "erreur"
				          aux1 rest
				        in aux1 (List.tl t);
		          let tps = temps rep2 mtr (voit_fait+1) in
		          if (tps < !min)
		              then begin min:=tps; min_t:=t; end;
					aux0 lttt
				in aux0 lt;
        let lieu_1 = ref dep in
        	let rec aux2 (traj:trajet) : unit =
        		match traj with
        		|[] -> ()
        		|lieu_2::reste ->
		          match repp.(!lieu_1).(lieu_2) with
		          |Some(nb_voi) ->
		          	repp.(!lieu_1).(lieu_2) <- Some(nb_voi+1);
		          	lieu_1:=lieu_2;
		          |None -> failwith "erreur"
		       	aux2 reste;
		      in aux2 (List.tl !min_t);
        if ((voit_fait+1) mod ecart = 0) then
            aux repp (voit_fait+1)(((voit_fait+1),(temps repp mtr (voit_fait+1)))::tps_l)
        else aux repp (voit_fait+1) tps_l
    end
    in aux rep 0 []
*)
(*

let rep_social (mtr:(int->float) option array array) (lt:trajet list) (nb_voit:int) =
(*Retourne la répartition sociale pour nb_voiture  -> Matrice nb_lieu nb_lieu avec None s'il n'y a pas de route entre deux lieu et Some(voit) sinon, où voit est le nombre de voitures qui emprunte la route*)
    let rep : int option array array = Array.make_matrix nb_lieu nb_lieu None in
    for i = 0 to nb_lieu-1 do
        for j = 0 to nb_lieu-1 do
            match mtr.(i).(j) with
            |None -> rep.(i).(j) <- None
            |Some(_) -> rep.(i).(j) <- Some(0)
        done;
    done;
    for e = 0 to nb_voit-1 do (* Pour toutes les voitures *)
        let traj_min = ref [] in
        let min_t = ref (10.**16.) in
        let rep_min = ref(Array.make_matrix nb_lieu nb_lieu None) in
          let rec aux1 (ltt:trajet list) : unit =
          	match ltt with
          	|[] -> ()
          	|t::lttt ->
            	let lieu_1 = ref dep in
          		let rec aux2 (tr:trajet) : unit =
          			let rep_provi = Array.copy !rep in
          			match tr with
          				|[] -> ()
          				|lieu_2::ttr -> begin
          					match rep_provi.(!lieu_1).(lieu_2) with
          					|None -> rep_provi.(!lieu_1).(lieu_2) <-Some(1)
          					|Some(nb) -> rep_provi.(!lieu_1).(lieu_2) <- Some(nb + 1)
          				end;
          			let tps = temps rep_provi mtr (e+1) in
          			if tps < !min_t then begin
          				min_t := tps;traj_min := tr;rep_min := rep_provi;aux2 ttr;
          			end
          			else aux2 ttr
	            in aux2 (List.tl t);
            	aux1 lttt
          in aux1 lt;
          rep:=!rep_min;
        if ((e+1) mod ecart = 0) then
         tps_l:=(e+1,!min_t)::(!tps_l)
        else ()
    done;
    List.rev (!tps_l)
*)



let liste_temps_rep_social (mtr:(int->float) option array array)(lt:trajet list)(ecart:int)(voit_max:int) =
    let r = ref(rep_social mtr lt 1) in
    let ltrs = ref([(1,temps !r mtr 1)]) in
    for e = 1 to voit_max-1 do (* Pour toutes les voitures *)
    	print_int e; print_newline();
    		r :=rep_social_plus_1 !r mtr lt e;
        ltrs := (e+1, temps !r mtr (e+1))::!ltrs;
    done;
    List.rev !ltrs
     


let liste_prix_anarchie (ltre:(int*float) list) (ltrs:(int*float) list) : (int*float) list =
    let l = ref [] in
    for i = 0 to (List.length ltre)-1 do
        let (nb1,t1) = List.nth ltre i in
        let (nb2,t2) = List.nth ltrs i in
        l:=(nb1,t1/.t2)::(!l);
    done;
    List.rev !l


(*
let liste_diff_prix_anarchie_sans_i (r_route:rep_route)(voit_max:int)(i:int) : (int*float) list =
	let g = graphe_trajet r_route in
	let g_sans_i = graphe_trajet_sans_i r_route i in
	let lt = rem_liste (liste_trajet g) in
	let lt_sans_i = rem_liste (liste_trajet g) in
	let ltre = liste_temps_rep_ego (mat_temps_route g) lt 1 nb_voiture in
	let ltre_sans_i = liste_temps_rep_social (mat_temps_route g_sans_i) lt_sans_i 1 nb_voiture in
	let ltrs = liste_temps_rep_social (mat_temps_route g) lt 1 nb_voiture in
	let ltrs_sans_i = liste_temps_rep_social (mat_temps_route g_sans_i) lt_sans_i 1 nb_voiture in
	let lpa = liste_prix_anarchie ltre ltrs in
	let lpa_sans_i = liste_prix_anarchie ltre_sans_i ltrs_sans_i in
	let liste = ref [] in
	for i = 0 to voit_max-1 do
		liste:=(i,(snd(List.nth lpa_sans_i i))-.snd((List.nth lpa i)))::!liste
	done;
	List.rev !liste
*)



let prix_anarchie_max (lpa:(int*float) list) : (int*float) =
    let max = ref 0. in
    let max_nb = ref 0 in
    for i = 0 to (List.length lpa)-1 do
        let (nb,t) = List.nth lpa i in
        if t > !max
            then begin max:=t; max_nb:=nb end
        else ()
    done;
    (!max_nb,!max)
    
let prix_anarchie_moy (lpa:(int*float) list) : float =
    let som = ref 0. in
    for i = 0 to (List.length lpa)-1 do
        let (nb,t) = List.nth lpa i in
        som := !som +. t
    done;
    !som /. float_of_int(List.length lpa)

let prix_anarchie_modif (lpa:(int*float) list) : int =
    let nb_min = ref (List.length lpa) in
    for i = 0 to (List.length lpa)-1 do
        let (v,t) = List.nth lpa i in
        if (t=1.) then ()
        else
            begin
                if (v < !nb_min) then  nb_min := v
                else ()
            end
    done;
    !nb_min






let g = graphe_trajet r_route    
let lt = min_list 20 (rem_liste (liste_trajet g))

let mtr = mat_temps_route g

let ltre = liste_temps_rep_ego mtr lt nb_voiture
let ltrs = liste_temps_rep_social mtr lt 1 nb_voiture


let lpa = liste_prix_anarchie ltre ltrs
let (pama_v,pama_t) = prix_anarchie_max lpa
let pamo = prix_anarchie_moy lpa
let padiff = (prix_anarchie_modif lpa)
let ec = 152(*
    if (padiff<300) then 0
    else padiff/2*)
let padiff2 = 305(*
    if ec = 0 then 0 else padiff*)

let u = Graphics.open_graph ""

let mv x y = Graphics.moveto x y
let mv2 x y = Graphics.lineto x y

let f1 x =
    mv 90 (150+x*150);
    mv2 110 (150+x*150)

let f2 x =
    mv (200+x*100) 110;
    mv2 (200+x*100) 90


let calc_x a =
    int_of_float(100. +. (float_of_int(a-padiff2+ec)/.float_of_int(nb_voiture-padiff2+ec))*.1200.)
           
let calc_y t =
    int_of_float(150. +. (t-.1.)*.(600.)/.(pama_t-.1.))       

let x_max = (calc_x pama_v)
let y_max = (calc_y pama_t) + 20

let param =
    Graphics.set_window_title "Prix de l'anarchie en fonction du nombre de voitures";
    Graphics.resize_window 1500 900;
    mv 100 100;
    mv2 1400 100;
    mv2 1360 120;
    mv 1360 80;
    mv2 1400 100;
    mv 100 800;
    mv2 80 760;
    mv 120 760;
    mv2 100 800;
    mv2 100 100;
    for i=0 to 4 do
        f1 i;
        mv (Graphics.current_x ()-45) (Graphics.current_y ()-5);
        Graphics.draw_string(string_of_float(1.+.((((Float.round(pama_t*.100.))/.100.)-.1.)/.4.)*.float_of_int(i)));
    done;
    for j=0 to 11 do
        f2 j;
        mv (Graphics.current_x ()-5) (Graphics.current_y ()-20);
        Graphics.draw_string(string_of_int(((nb_voiture-padiff2+ec)/12)*(j+1)+(padiff2-ec)));
    done;
    mv 95 75;
    Graphics.draw_string(string_of_int(padiff2-ec));
    mv 1300 50;
    Graphics.draw_string("Nombre de voitures");
    mv 65 830;
    Graphics.draw_string("Prix de l'anarchie");
    mv 100 150;
    Graphics.set_color 180
    

let stop x = Graphics.close_graph x
(*Pour arrêter le graphique, SURTOUT ne pas fermer la fenêtre, utiliser plutôt stop u;;*)


let flechemax =
    mv x_max y_max;
    Graphics.lineto(x_max-20)(y_max+30);
    mv x_max y_max;
    Graphics.lineto(x_max+20)(y_max+30);
    mv x_max y_max;
    Graphics.lineto(x_max)(y_max+60);
    mv (x_max - 115) (y_max + 100);
    Graphics.draw_string("Le prix de l'anarchie maximum est : ");
    Graphics.draw_string(string_of_float((Float.round(pama_t*.100.))/.100.));
    mv (x_max - 115) (y_max + 80);
    Graphics.draw_string("Il est atteint pour ");
    Graphics.draw_string(string_of_int(pama_v));
    Graphics.draw_string(" voitures.")

    let param2 =
        mv 100 150;
        for i = (padiff2-ec) to (nb_voiture-1) do
            let (v,t) = List.nth lpa i in
            if (i mod ((nb_voiture-padiff2+ec)/240 + 1) = 0) then
                mv2 (calc_x v) (calc_y (t))
            else ();
            if (i mod ((nb_voiture-padiff2+ec)/240 + 1) = 0) then
                (Graphics.set_color 255;
                Graphics.fill_circle (calc_x v) (calc_y (t)) 5;
                Graphics.set_color 180)
        else ()
    done;

(*
    let param2 =
        mv 100 150;
        for i = (padiff2-ec) to (nb_voiture-1) do
            let (v,t) = List.nth lpa i in
            if (i mod 2 = 0) then
                mv2 (calc_x v) (calc_y (t))
            else ();
            if (i mod 2 = 0) then
                (Graphics.set_color 255;
                Graphics.fill_circle (calc_x v) (calc_y (t)) 5;
                Graphics.set_color 180)
        else ()
    done;
    *)
