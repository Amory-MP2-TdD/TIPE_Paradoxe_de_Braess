#require "graphics"
#load "graphics.cma"

(*Tas de Fibonacci*)


type lieu = int						(* numéro du lieu *)
type num_route = int			(* numéro de route *)
type route = (lieu*lieu*float*int) 			(* (dep,arr,tps de + par voit,tps init) *)
type trajet = num_route list					
type num_trajet = int
type tab_route = route array				(* tableau des routes à considéré *)
type tab_trajet = trajet array
type mat_adj_indice = int option array array	(* matrice d'adjacence representant l'ensemble des routes *)
type mat_adj_fonction_temps = (int->float) option array array
type repartition_trajet = int array

type 'a liste_doublement_chainee_circulaire = {
	t : 'a array;
	max : int; (* taille de t *)
	mutable premier : int;
	mutable nombre : int}

type 'a arbre =
	|Vide
	|Noeud of {mutable elem:'a ; mutable prio:float ; mutable fils:('a arbre) liste_doublement_chainee_circulaire ; mutable pere:'a ; mutable marque:bool}

type 'a fibo = {
	table : ('a,'a arbre) Hashtbl.t;
	mutable liste_arbres : ('a arbre) liste_doublement_chainee_circulaire;
	mutable minimum : 'a arbre}
	
type repartition_route_direct = (num_route*int*int Queue.t*int fibo)fibo

let cree_ldcc_vide (m:int) (e:'a) : 'a liste_doublement_chainee_circulaire = 
	{t = Array.make m e ; max = m ; premier = 0 ; nombre = 0}

let ajoute_ldcc (e:'a) (l:'a liste_doublement_chainee_circulaire) : unit =
	if l.nombre = l.max then failwith "pas assez de place"
	else begin
		l.nombre <- l.nombre + 1;
		if l.premier + l.nombre > l.max then
			l.t.(l.premier + l.nombre - l.max - 1) <- e
		else
			l.t.(l.premier + l.nombre - 1) <- e
	end

exception Probleme

let supprime_ldcc (e:'a) (l:'a liste_doublement_chainee_circulaire) : unit =
	let i = ref l.premier in
	let j = ref 0 in
	while l.t.(!i) <> e && !j <= l.nombre do
		j:=!j+1;
		if !i+1 < l.max then i := !i + 1
		else i := 0
	done;
	if !j>l.nombre then raise (Probleme)
	else
		l.t.(!i) <- l.t.(l.premier);
		l.nombre <- l.nombre - 1;
		if l.premier + 1 < l.max then l.premier <- l.premier + 1
		else l.premier <- 0



let cree_fibo_vide (m:int) : 'a fibo =
	{table = Hashtbl.create m ; liste_arbres = cree_ldcc_vide m Vide ; minimum=Vide}

(*
let optimise_fibo (f:'a fibo) : unit =
	let p = f.liste_arbres.premier in 
	let d = ref 0 in
	begin
	if p + f.liste_arbres.nombre >= f.liste_arbres.max then
		d := p + f.liste_arbres.nombre - f.liste_arbres.max
	else d := p + f.liste_arbres.nombre
	end;
	if !d < p then
		for i = !d + 2 to p-2 do
			f.liste_arbres.t.(i) <- Vide
		done
	else
		for i = 1 to p-2 do
			f.liste_arbres.t.(i) <- Vide
		done;
		for i = !d + 2 to f.liste_arbres.max - 1 do
			f.liste_arbres.t.(i) <- Vide
		done
*)

let union_fibo (f:'a fibo) (g:'a fibo) : unit =
	(* f ou g n'est pas vide *)
	begin
	match f.minimum,g.minimum with
	|Noeud n1, Noeud n2 when n1.prio > n2.prio -> f.minimum <- g.minimum
	|Noeud n1, _-> ()
	|_ -> f.minimum <- g.minimum
	end;
	if f.liste_arbres.nombre + g.liste_arbres.nombre > f.liste_arbres.max then failwith "pas assez de place dans fibo"
	else
		for i = 0 to g.liste_arbres.nombre - 1 do
			ajoute_ldcc g.liste_arbres.t.(i) f.liste_arbres
		done;
	Hashtbl.iter (fun i a -> Hashtbl.add f.table i a) g.table




let ajoute_fibo (e:'a) (p:float) (f:'a fibo) : unit =
	let f_bis = cree_fibo_vide 1 in
	let arbre = Noeud {elem=e ; prio=p ; fils = cree_ldcc_vide 10 Vide ; pere = e ; marque = false} in
	f_bis.minimum <- arbre;
	f_bis.liste_arbres.nombre <- 1;
	f_bis.liste_arbres.t.(0) <- arbre;
	Hashtbl.add f.table e arbre;
	union_fibo f f_bis
	
let obtenir_min_fibo_elem (f:'a fibo) : 'a =
	match f.minimum with
	|Vide -> failwith "elem : le tas est vide donc il n'y a pas de minimum"
	|Noeud{elem=e} -> e

let obtenir_min_fibo_prio (f:'a fibo) : float =
	match f.minimum with
	|Vide -> Float.infinity
	|Noeud{prio=p} -> p


let degre_arbre (a:'a arbre) =
	match a with
	|Vide -> 0
	|Noeud{elem=e ; prio=p ; fils = l} -> l.nombre


let extraire_min_fibo (f:'a fibo) : 'a =
	supprime_ldcc f.minimum f.liste_arbres;
	match f.minimum with
		|Vide -> failwith "erreur15"
		|Noeud n ->
			let mini = n.elem in
			begin
			let j = ref n.fils.premier in
			for i = 0 to degre_arbre f.minimum - 1 do
				match n.fils.t.(!j) with
				|Vide -> failwith "erreur16"
				|Noeud n_fils ->
					n_fils.pere <- n_fils.elem;
					ajoute_ldcc (Noeud n_fils) f.liste_arbres;
					Hashtbl.remove f.table n_fils.elem;
					Hashtbl.add f.table n_fils.elem (Noeud n_fils);
				if !j + 1 < n.fils.max then j := !j + 1
				else j := 0
			done;
			end;
	let tab = Array.make f.liste_arbres.max Vide in
	let j = ref f.liste_arbres.premier in
	for i = 0 to f.liste_arbres.nombre - 1 do
		let rec aux (arb:'a arbre) : unit = 
			match tab.(degre_arbre arb) with
			|Vide -> tab.(degre_arbre arb) <- arb
			|Noeud m ->
				supprime_ldcc (Noeud m) f.liste_arbres;
				supprime_ldcc arb f.liste_arbres;
				match arb with
				|Vide -> failwith "erreur17"
				|Noeud a when a.prio < m.prio ->
					m.pere <- a.elem;
					a.pere <- a.elem;
					ajoute_ldcc (Noeud m) a.fils;
					ajoute_ldcc (Noeud a) f.liste_arbres;
					tab.(degre_arbre (Noeud a) - 1) <- Vide;
					aux (Noeud a)
				|Noeud a ->
					a.pere <- m.elem;
					m.pere <- m.elem;
					ajoute_ldcc (Noeud a) m.fils;
					ajoute_ldcc (Noeud m) f.liste_arbres;
					tab.(degre_arbre (Noeud m) - 1) <- Vide;
					aux (Noeud m)
		in aux f.liste_arbres.t.(!j);
		if !j + 1 < f.liste_arbres.max then j := !j + 1
		else j := 0
	done;
	let min_j = ref mini in
	let min_p = ref Float.infinity in
	let j = ref f.liste_arbres.premier in
	for i = 0 to f.liste_arbres.nombre - 1 do
		match f.liste_arbres.t.(!j) with
		|Vide -> ()
		|Noeud n -> 
			Hashtbl.remove f.table n.elem;
			Hashtbl.add f.table n.elem (Noeud n);
			if !min_p >= n.prio then begin
				min_p := n.prio;
				min_j := n.elem
			end;
			if !j + 1 < f.liste_arbres.max then j := !j + 1
			else j := 0
	done;
	if !min_j = mini then f.minimum <- Vide
	else f.minimum <- Hashtbl.find f.table !min_j;
	mini


let diminuer_prio_fibo (e:'a) (p:float) (f:'a fibo) : unit =
	match Hashtbl.find f.table e with
	|Vide -> failwith "erreur18"
	|Noeud n ->
		begin
		match f.minimum with
		|Vide -> failwith "erreur19"
		|Noeud mi when mi.prio < p -> ()
		|_ -> f.minimum <- Noeud n
		end;
		n.prio <- p;
		let continu = ref true in
		let n_pere = ref n.pere in
		let fils = ref (Noeud n) in
		while !continu do
			match Hashtbl.find f.table !n_pere with
			|Vide -> failwith "erreur20"
			|Noeud nn when nn.prio <= n.prio -> continu := false
			|Noeud nn ->
				supprime_ldcc (!fils) nn.fils;
				match !fils with
				|Vide -> failwith "erreur20.5"
				|Noeud m ->
					m.pere <- m.elem;
					ajoute_ldcc (Noeud m) f.liste_arbres;
					Hashtbl.remove f.table m.elem;
					Hashtbl.add f.table m.elem (Noeud m);
					if nn.elem <> nn.pere then
						if nn.marque then begin
							fils := Noeud nn;
							n_pere := nn.pere
							end
						else begin 
							nn.marque <- true;
							continu := false
						end							
					else continu := false
		done;
		match f.minimum with
		|Vide -> failwith "erreur21"
		|Noeud mi -> f.minimum <- Hashtbl.find f.table mi.elem



let supprime_element_fibo (e:'a) (f:'a fibo) : unit =
	diminuer_prio_fibo e Float.neg_infinity f;
	let _ = extraire_min_fibo f in
	()

let augmenter_prio_fibo (e:'a) (p:float) (f:'a fibo) : unit =
	supprime_element_fibo e f;
	ajoute_fibo e p f
	
let f = 
	let g = cree_fibo_vide 15 in
	ajoute_fibo 1 1. g;
	ajoute_fibo 2 2. g;
	ajoute_fibo 3 3. g;
	ajoute_fibo 4 4. g;
	ajoute_fibo 5 5. g;
	g


let est_vide_fibo (f:'a fibo) : bool =
	f.liste_arbres.nombre = 0
	
	

(*
let tab_routes : tab_route = [|(0,1,0.,40);(2,3,0.,40);(0,2,1.,0);(1,3,1.,0);(2,1,0.,0);(1,2,0.,0)|]
let nb_lieu = 4
let dep = 0
let arr = 3
let voit_tot = 40

let fonction_temps_route (t:tab_route)(i:num_route)(x:int) =			(* calcule le temps d'emprunt de la route i de  *)
let (_,_,a,b) = tab_routes.(i) in
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

let voit_tot = 1
let nb_lieu = 49
(*
let dep = 0
let arr = 26 (*25 cool*) (*39 montagne*)
*)

let tab_routes : tab_route = [|
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
(17,48,om,150)|]


let fonction_temps_route (t:tab_route)(i:num_route)(x:int) =			(* calcule le temps d'emprunt de la route i par x voitures *)
let (_,_,a,b) = tab_routes.(i) in
(float_of_int(b)/.30.)*.(1.+.0.2*.(float_of_int(x)/.a)**10.)							


let graphe_trajet (t:tab_route) : mat_adj_indice =		(* cree la matrice d'adjacence du graphe, matrice indicé par le numéro des routes *)
	let tab = Array.make_matrix nb_lieu nb_lieu None in
		for i=0 to Array.length t - 1 do
			let (dep,arr,a,b) = t.(i) in
			tab.(dep).(arr) <- Some(i)
		done;
    tab

    
let graphe_trajet_sans_i (t:tab_route) (i:num_route) : mat_adj_indice =		(* cree la matrice d'adjacence du graphe privé de la route i, matrice indicé par le numéro des routes *)
	let tab = Array.make_matrix nb_lieu nb_lieu None in
		for j=0 to nb_lieu-1 do
			if j<>i then begin
				let (dep,arr,a,b) = t.(j) in
				tab.(dep).(arr) <- Some(j)
			end;
		done;
    tab





let tab_trajet (m:mat_adj_indice) (dep:lieu) (arr:lieu) (t:tab_route) : tab_trajet =								(* renvoie le tableau des trajets possibles entre dep et arr d'après m *)
	let liste_traj = ref [] in
	let lieu_visite = Array.make nb_lieu false in
	lieu_visite.(dep) <- true;
	let rec aux (l_actu:lieu) (traj_actu:trajet) (visite:bool array)  =
		for i = 0 to nb_lieu-1 do
			match m.(l_actu).(i),visite.(i) with
			|_,true -> ();
			|None,false -> ();
			|Some(n_route),false ->
				if i = arr then liste_traj := (List.rev (n_route::traj_actu))::!liste_traj
				else
					if (List.mem n_route traj_actu) then ()
					else begin
						let visite_bis = Array.copy visite in
						visite_bis.(i) <- true;
						aux i (n_route::traj_actu) visite_bis
					end;
		done;
	in aux dep [] lieu_visite;
	Array.of_list (!liste_traj)






let mat_temps_route (m:mat_adj_indice)(t:tab_route) : mat_adj_fonction_temps =
	let tab = Array.make_matrix nb_lieu nb_lieu None in
	for i = 0 to nb_lieu-1 do
		for j = 0 to nb_lieu-1 do
			match m.(i).(j) with
			|None -> tab.(i).(j) <- None
			|Some(n_route) -> tab.(i).(j)<-Some(fonction_temps_route t n_route)
		done;
	done;
	tab

let reduc_tab_traj (tab:(num_trajet list) array) (n:int) =
	let lt = Array.to_list tab in
	Array.of_list(List.filter (fun l -> List.length l <= n) lt)
	

(*V1 : On découpe le temps en paquet et on compte le nombre de voiture qui sontsur la route dans le paquet*)




let temps (r:repartition_trajet)(mf:mat_adj_fonction_temps)(tab_traj:tab_trajet)(tab_routes:tab_route) (voit_tot:int)= (*calcule le temps de trajet associé à la répartition mv*)
	let nb_routes = Array.length tab_routes in
	let nb_traj = Array.length tab_traj in
	let tps = ref 0. in
	let tps_tot = ref 0. in
	let nb_voit = ref 0 in
	Array.iter (fun x -> nb_voit := !nb_voit + x) r;
	let init_route = Array.make nb_routes 0 in
	let tab_t = Array.copy tab_traj in
	let table = Array.make nb_routes None in
	let utile = ref 0 in
	let rep = Array.copy r in
	for i = 0 to nb_traj - 1 do
		if rep.(i) <> 0 then utile := !utile + 1;
		match tab_t.(i) with
		|[] -> failwith "erreur1"
		|e::suiv ->
			init_route.(e) <- init_route.(e) + rep.(i);
	done;
	let r_routes : repartition_route_direct = cree_fibo_vide !utile in
	for i = 0 to nb_routes - 1 do
		let (d,a,_,_) = tab_routes.(i) in
		match init_route.(i),mf.(d).(a) with
		|0,_ -> ()
		|n,None -> failwith "erreur2"
		|n,Some(f) ->
			let v = (i,n,Queue.create(),cree_fibo_vide !utile) in
			ajoute_fibo v (f n) r_routes;
			table.(i) <- Some(v)
	done;
	for i = 0 to nb_traj - 1 do
		match rep.(i),tab_t.(i) with
		|0,_ -> tab_t.(i) <- []
		|_,[] -> failwith "erreur3"
		|n,e::suiv ->
			let (d,a,_,_) = tab_routes.(e) in
			begin
			match mf.(d).(a) with
			|None -> failwith "erreur4"
			|Some(f) ->
				match table.(e) with
				|Some(aa) ->
					let (nr,nv,q,fib) = aa in
					supprime_element_fibo (aa) r_routes;
					ajoute_fibo i (f init_route.(e)) fib;
					ajoute_fibo (nr,nv,q,fib) (obtenir_min_fibo_prio fib) r_routes;
					table.(e) <- Some(nr,nv,q,fib)
				|_ -> failwith "erreur5"
			end;
			tab_t.(i) <- suiv
	done;
	let dans_q = Array.make nb_routes false in
	let comp = ref 0 in
	while not (est_vide_fibo r_routes) do
		(*print_int !comp;
		print_string" : ";
		print_float !tps_tot;
		print_newline();
		comp:=!comp+1;*)
		let continu = ref true in
		while !continu do
			let (n_route,nb_voit,q,fib) = extraire_min_fibo r_routes in
			if est_vide_fibo fib then ()
			else begin
			match fib.minimum with
			|Vide -> failwith "erreur6"
			|Noeud e ->
				tps := e.prio;
				let _ = extraire_min_fibo fib in
				if est_vide_fibo fib && Queue.is_empty q then table.(n_route) <- None;
				match tab_t.(e.elem) with
				|[] -> if table.(n_route)<>None then begin 
					ajoute_fibo (n_route,nb_voit-rep.(e.elem),q,fib) (obtenir_min_fibo_prio fib) r_routes;
					match table.(n_route) with
					|None -> failwith "erreur7"
					|Some (a,b,c,d) -> table.(n_route) <- Some (a,nb_voit - rep.(e.elem),c,d)
					end;
					(*print_int e.elem;
					print_string" (";
					print_int rep.(e.elem);
					print_string "), tps : ";
					print_float !tps;
					print_string", tps_tot : ";
					print_float !tps_tot;
					print_newline();*)
					tps_tot := !tps_tot +. (!tps *. float_of_int (rep.(e.elem)));
					(*print_float !tps_tot;
					print_newline()*)
				|i::suiv ->
					(*print_string "b";*)
					tab_t.(e.elem) <- suiv;
					let (a,b,_,_) = tab_routes.(i) in
					match mf.(a).(b) with
					|None -> failwith "erreur8"
					|Some f ->
						match table.(i) with
						|None ->
							let qq = Queue.create() in
							Queue.add e.elem qq;
							let v = (i,0,qq,cree_fibo_vide !utile) in
							ajoute_fibo v Float.infinity r_routes;
							table.(i) <- Some(v);
							dans_q.(i) <- true;
							if (nb_voit <> rep.(e.elem)) && (not (est_vide_fibo fib)) then begin
								ajoute_fibo (n_route,nb_voit - rep.(e.elem),q,fib) (obtenir_min_fibo_prio fib) r_routes;
								match table.(n_route) with
								|None -> failwith "erreur9"
								|Some (a,b,c,d) -> table.(n_route) <- Some (a,nb_voit - rep.(e.elem),c,d)
								end
							else table.(n_route) <- None
						|Some(v) ->
							let (r,voit,qq,fi) = v in
							let k = Hashtbl.find r_routes.table v in
							Hashtbl.remove r_routes.table v;
							Queue.add e.elem qq;
							table.(i) <- Some(v);
							Hashtbl.add r_routes.table v k;
							dans_q.(i) <- true;
							if (nb_voit <> rep.(e.elem)) && (not (est_vide_fibo fib)) then begin
							ajoute_fibo (n_route,nb_voit - rep.(e.elem),q,fib) (obtenir_min_fibo_prio fib) r_routes;
								match table.(n_route) with
								|None -> failwith "erreur10"
								|Some (a,b,c,d) -> table.(n_route) <- Some (a,nb_voit - rep.(e.elem),c,d)
								end
							else table.(n_route) <- None
					end;
				if est_vide_fibo r_routes then continu := false
				else if obtenir_min_fibo_prio r_routes > !tps then continu := false
		done;
		for i = 0 to nb_routes - 1 do
			match dans_q.(i) with
			|false -> ()
			|true -> begin
				dans_q.(i) <- false;
				match table.(i) with
				|None -> failwith "erreur11"
				|Some e ->
					match Hashtbl.find r_routes.table e with
					|Vide -> failwith "erreur12"
					|Noeud n ->
						let (nr,nv,q,fib) = n.elem in
						let traj = ref (Queue.pop q) in
						let prem = !traj in
						let somme = ref (nv + rep.(!traj)) in
						Queue.add (!traj) q;
						while Queue.top q <> prem do
							traj := Queue.pop q;
							Queue.add (!traj) q;
							somme := !somme + rep.(!traj)
						done;
						supprime_element_fibo n.elem r_routes;
						let (a,b,_,_) = tab_routes.(nr) in
						match mf.(a).(b) with
						|None -> failwith "erreur13"
						|Some f ->
							while not (Queue.is_empty q) do
								ajoute_fibo (Queue.pop q) (!tps +. f !somme) fib
							done;
							let ppp = obtenir_min_fibo_prio fib in
							ajoute_fibo (nr,!somme,q,fib) ppp r_routes;
							begin
							match table.(nr) with
							|None -> failwith "erreur14"
							|Some (a,b,c,d) -> table.(nr) <- Some (a,!somme,c,d)
							end;
							Hashtbl.remove r_routes.table e;
							Hashtbl.add r_routes.table e (Noeud n);
			end;
		done;
	done;
	!tps_tot /. (float_of_int voit_tot)




let deb (rep:repartition_trajet)(mf:mat_adj_fonction_temps)(tab_traj:tab_trajet)(tab_routes:tab_route) = (*calcule le temps de trajet associé à la répartition mv*)
	let nb_routes = Array.length tab_routes in
	let nb_traj = Array.length rep in
	let tps = ref 0. in
	let init_route = Array.make nb_routes 0 in
	let tab_t = Array.copy tab_traj in
	let table = Array.make nb_routes None in
	let utile = ref 0 in
	for i = 0 to nb_traj - 1 do
		if rep.(i) <> 0 then utile := !utile + 1;
		match tab_t.(i) with
		|[] -> failwith "erreur"
		|e::suiv -> 
			init_route.(e) <- init_route.(e) + rep.(i);	
	done;
	let r_routes : repartition_route_direct = cree_fibo_vide !utile in
	for i = 0 to nb_routes - 1 do
		let (d,a,_,_) = tab_routes.(i) in
		match init_route.(i),mf.(d).(a) with
		|0,_ -> ()
		|n,None -> failwith "erreur"
		|n,Some(f) ->
			let v = (i,n,Queue.create(),cree_fibo_vide !utile) in
			ajoute_fibo v (f n) r_routes;
			table.(i) <- Some(v)
	done;
	for i = 0 to nb_traj - 1 do
		match rep.(i),tab_t.(i) with
		|0,_ -> tab_t.(i) <- []
		|_,[] -> failwith "erreur"
		|n,e::suiv ->
			let (d,a,_,_) = tab_routes.(e) in
			begin
			match mf.(d).(a) with
			|None -> failwith "erreur"
			|Some(f) ->
				match table.(e) with
				|Some(aa) ->
					let (nr,nv,q,fib) = aa in
					supprime_element_fibo (aa) r_routes;
					ajoute_fibo i (f init_route.(e)) fib;
					ajoute_fibo (nr,nv,q,fib) (obtenir_min_fibo_prio fib) r_routes;
					table.(e) <- Some(nr,nv,q,fib)
				|_ -> failwith "erreur"
			end;
			tab_t.(i) <- suiv
	done;
	let dans_q = Array.make nb_routes false in
	tps,r_routes,table,dans_q,tab_t,tab_routes,mf,rep,nb_routes,!utile



let mid (tps:'a) (r_routes:'b) (table:'c) (dans_q:'d) (tab_t:'e) (tab_routes:'f) (mf:'g) (rep:'h) (nb_routes:'i) (utile:'j) =
	let continu = ref true in
	while !continu do
		let (n_route,nb_voit,q,fib) = extraire_min_fibo r_routes in
		if est_vide_fibo fib then ()
		else begin
		match fib.minimum with
		|Vide -> failwith "erreur6"
		|Noeud e ->
			tps := e.prio;
			let _ = extraire_min_fibo fib in
			if est_vide_fibo fib && Queue.is_empty q then table.(n_route) <- None;
			match tab_t.(e.elem) with
			|[] -> if table.(n_route)<>None then begin 
				ajoute_fibo (n_route,nb_voit-rep.(e.elem),q,fib) (obtenir_min_fibo_prio fib) r_routes;
				match table.(n_route) with
				|None -> failwith "erreur7"
				|Some (a,b,c,d) -> table.(n_route) <- Some (a,nb_voit - rep.(e.elem),c,d)
				end
			|i::suiv ->
				tab_t.(e.elem) <- suiv;
				let (a,b,_,_) = tab_routes.(i) in
				match mf.(a).(b) with
				|None -> failwith "erreur8"
				|Some f ->
					match table.(i) with
					|None ->
						let qq = Queue.create() in
						Queue.add e.elem qq;
						let v = (i,0,qq,cree_fibo_vide utile) in
						ajoute_fibo v Float.infinity r_routes;
						table.(i) <- Some(v);
						dans_q.(i) <- true;
						if (nb_voit <> rep.(e.elem)) && (not (est_vide_fibo fib)) then begin
							ajoute_fibo (n_route,nb_voit - rep.(e.elem),q,fib) (obtenir_min_fibo_prio fib) r_routes;
							match table.(n_route) with
							|None -> failwith "erreur9"
							|Some (a,b,c,d) -> table.(n_route) <- Some (a,nb_voit - rep.(e.elem),c,d)
							end
						else table.(n_route) <- None
					|Some(v) ->
						let (r,voit,qq,fi) = v in
						let k = Hashtbl.find r_routes.table v in
						Hashtbl.remove r_routes.table v;
						Queue.add e.elem qq;
						table.(i) <- Some(v);
						Hashtbl.add r_routes.table v k;
						dans_q.(i) <- true;
						if (nb_voit <> rep.(e.elem)) && (not (est_vide_fibo fib)) then begin
						ajoute_fibo (n_route,nb_voit - rep.(e.elem),q,fib) (obtenir_min_fibo_prio fib) r_routes;
							match table.(n_route) with
							|None -> failwith "erreur10"
							|Some (a,b,c,d) -> table.(n_route) <- Some (a,nb_voit - rep.(e.elem),c,d)
							end
						else table.(n_route) <- None
				end;
			if est_vide_fibo r_routes then continu := false
			else if obtenir_min_fibo_prio r_routes > !tps then continu := false
	done;	
	tps,r_routes,table,dans_q,tab_t,tab_routes,mf,rep,nb_routes,utile

let fin (tps:'a) (r_routes:'b) (table:'c) (dans_q:'d) (tab_t:'e) (tab_routes:'f) (mf:'g) (rep:'h) (nb_routes:'i) (utile:'j)=
		for i = 0 to nb_routes - 1 do
			match dans_q.(i) with
			|false -> ()
			|true -> begin
				dans_q.(i) <- false;
				match table.(i) with
				|None -> failwith "erreur"
				|Some e ->
					match Hashtbl.find r_routes.table e with
					|Vide -> failwith "erreur"
					|Noeud n ->
						let (nr,nv,q,fib) = n.elem in
						let traj = ref (Queue.pop q) in
						let prem = !traj in
						let somme = ref (nv + rep.(!traj)) in
						Queue.add (!traj) q;
						while Queue.top q <> prem do
							traj := Queue.pop q;
							Queue.add (!traj) q;
							somme := !somme + rep.(!traj)
						done;
						supprime_element_fibo n.elem r_routes;
						let (a,b,_,_) = tab_routes.(nr) in
						match mf.(a).(b) with
						|None -> failwith "erreur"
						|Some f ->
							while not (Queue.is_empty q) do
								ajoute_fibo (Queue.pop q) (!tps +. f !somme) fib;
							done;
							let ppp = obtenir_min_fibo_prio fib in
							ajoute_fibo (nr,!somme,q,fib) ppp r_routes;
							begin
							match table.(nr) with
							|None -> failwith "erreur"
							|Some (a,b,c,d) -> table.(nr) <- Some (a,!somme,c,d)
							end;
							Hashtbl.remove r_routes.table e;
							Hashtbl.add r_routes.table e (Noeud n)
			end;
		done;
	tps,r_routes,table,dans_q,tab_t,tab_routes,mf,rep,nb_routes,utile




let midepe (tps:'a) (r_routes:'b) (table:'c) (dans_q:'d) (tab_t:'e) (tab_routes:'f) (mf:'g) (rep:'h) (nb_routes:'i) (utile:'j) =
	let (n_route,nb_voit,q,fib) = extraire_min_fibo r_routes in
		if est_vide_fibo fib then ()
		else begin
		match fib.minimum with
		|Vide -> failwith "erreur6"
		|Noeud e ->
			tps := e.prio;
			let _ = extraire_min_fibo fib in
			if est_vide_fibo fib && Queue.is_empty q then table.(n_route) <- None;
			match tab_t.(e.elem) with
			|[] -> if table.(n_route)<>None then begin 
				ajoute_fibo (n_route,nb_voit-rep.(e.elem),q,fib) (obtenir_min_fibo_prio fib) r_routes;
				match table.(n_route) with
				|None -> failwith "erreur7"
				|Some (a,b,c,d) -> table.(n_route) <- Some (a,nb_voit - rep.(e.elem),c,d)
				end
			|i::suiv ->
				tab_t.(e.elem) <- suiv;
				let (a,b,_,_) = tab_routes.(i) in
				match mf.(a).(b) with
				|None -> failwith "erreur8"
				|Some f ->
					match table.(i) with
					|None ->
						let qq = Queue.create() in
						Queue.add e.elem qq;
						let v = (i,0,qq,cree_fibo_vide utile) in
						ajoute_fibo v Float.infinity r_routes;
						table.(i) <- Some(v);
						dans_q.(i) <- true;
						if (nb_voit <> rep.(e.elem)) && (not (est_vide_fibo fib)) then begin
							ajoute_fibo (n_route,nb_voit - rep.(e.elem),q,fib) (obtenir_min_fibo_prio fib) r_routes;
							match table.(n_route) with
							|None -> failwith "erreur9"
							|Some (a,b,c,d) -> table.(n_route) <- Some (a,nb_voit - rep.(e.elem),c,d)
							end
						else table.(n_route) <- None
					|Some(v) ->
						let (r,voit,qq,fi) = v in
						let k = Hashtbl.find r_routes.table v in
						Hashtbl.remove r_routes.table v;
						Queue.add e.elem qq;
						table.(i) <- Some(v);
						Hashtbl.add r_routes.table v k;
						dans_q.(i) <- true;
						if (nb_voit <> rep.(e.elem)) && (not (est_vide_fibo fib)) then begin
						ajoute_fibo (n_route,nb_voit - rep.(e.elem),q,fib) (obtenir_min_fibo_prio fib) r_routes;
							match table.(n_route) with
							|None -> failwith "erreur10"
							|Some (a,b,c,d) -> table.(n_route) <- Some (a,nb_voit - rep.(e.elem),c,d)
							end
						else table.(n_route) <- None
				end;
	tps,r_routes,table,dans_q,tab_t,tab_routes,mf,rep,nb_routes,utile





let finepe (tps:'a) (r_routes:'b) (table:'c) (dans_q:'d) (tab_t:'e) (tab_routes:'f) (mf:'g) (rep:'h) (nb_routes:'i)(i:int) =
	dans_q.(i) <- false;
		match table.(i) with
		|None -> failwith "erreur"
		|Some e ->
			begin
			match Hashtbl.find r_routes.table e with
			|Vide -> failwith "erreur"
			|Noeud n ->
				let (nr,nv,q,fib) = n.elem in
				let traj = ref (Queue.pop q) in
				let prem = !traj in
				let somme = ref (nv + rep.(!traj)) in
				Queue.add (!traj) q;
				while Queue.top q <> prem do
					traj := Queue.pop q;
					Queue.add (!traj) q;
					somme := !somme + rep.(!traj)
				done;
				supprime_element_fibo n.elem r_routes;
				let (a,b,_,_) = tab_routes.(nr) in
				match mf.(a).(b) with
				|None -> failwith "erreur"
				|Some f ->
					while not (Queue.is_empty q) do
						ajoute_fibo (Queue.pop q) (!tps +. f !somme) fib;
					done;
					let ppp = obtenir_min_fibo_prio fib in
					ajoute_fibo (nr,!somme,q,fib) ppp r_routes;
					begin
					match table.(nr) with
					|None -> failwith "erreur"
					|Some (a,b,c,d) -> table.(nr) <- Some (a,!somme,c,d)
					end;
					Hashtbl.remove r_routes.table e;
					Hashtbl.add r_routes.table e (Noeud n);
			end;
	tps,r_routes,table,dans_q,tab_t,tab_routes,mf,rep,nb_routes



let a (rep:repartition_trajet)(mf:mat_adj_fonction_temps)(tab_traj:tab_trajet)(tab_routes:tab_route) =
	let  tps,r_routes,table,dans_q,tab_t,tab_routes,mf,rep,nb_routes,u = deb rep mf tab_traj tab_routes in
	for i = 0 to 88 do
		let tps,r_routes,table,dans_q,tab_t,tab_routes,mf,rep,nb_routes,u = mid tps r_routes table dans_q tab_t tab_routes mf rep nb_routes u in
		let tps,r_routes,table,dans_q,tab_t,tab_routes,mf,rep,nb_routes,u = fin tps r_routes table dans_q tab_t tab_routes mf rep nb_routes u in
		()
	done;
	tps,r_routes,table,dans_q,tab_t,tab_routes,mf,rep,nb_routes,u







let aaa (tab:int array)(t:tab_trajet)(tbase:tab_trajet) =
	for i = 0 to Array.length tab - 1 do
		if tab.(i) <> 0 then
			begin
			let traj = t.(i) in
			let j = ref 0 in
			while tbase.(!j) <> traj do
				j:=!j+1;
			done;
			print_int !j;
			print_string " : ";
			print_int tab.(i);
			print_newline()
			end;
	done

	


let temps_trajet (r:repartition_trajet)(mf:mat_adj_fonction_temps)(tab_traj:tab_trajet)(tab_routes:tab_route)(trajet:num_trajet) = 
	(*temps mais on arrête dès qu'il n'y a plus de voiture sur le trajet considéré*)
	let nb_routes = Array.length tab_routes in
	let nb_traj = Array.length tab_traj in
	let tps = ref 0. in
	let tps_tot = ref 0. in
	let init_route = Array.make nb_routes 0 in
	let tab_t = Array.copy tab_traj in
	let table = Array.make nb_routes None in
	let utile = ref 0 in
	let rep = Array.copy r in
	rep.(trajet) <- rep.(trajet) + 1;
	for i = 0 to nb_traj - 1 do
		if rep.(i) <> 0 then utile := !utile + 1;
		match tab_t.(i) with
		|[] -> failwith "erreur1"
		|e::suiv ->
			init_route.(e) <- init_route.(e) + rep.(i);	
	done;
	let r_routes : repartition_route_direct = cree_fibo_vide !utile in
	for i = 0 to nb_routes - 1 do
		let (d,a,_,_) = tab_routes.(i) in
		match init_route.(i),mf.(d).(a) with
		|0,_ -> ()
		|n,None -> failwith "erreur2"
		|n,Some(f) ->
			let v = (i,n,Queue.create(),cree_fibo_vide !utile) in
			ajoute_fibo v (f n) r_routes;
			table.(i) <- Some(v)
	done;
	for i = 0 to nb_traj - 1 do
		match rep.(i),tab_t.(i) with
		|0,_ -> tab_t.(i) <- []
		|_,[] -> failwith "erreur3"
		|n,e::suiv ->
			let (d,a,_,_) = tab_routes.(e) in
			begin
			match mf.(d).(a) with
			|None -> failwith "erreur4"
			|Some(f) ->
				match table.(e) with
				|Some(aa) ->
					let (nr,nv,q,fib) = aa in
					supprime_element_fibo aa r_routes;
					ajoute_fibo i (f init_route.(e)) fib;
					ajoute_fibo (nr,nv,q,fib) (obtenir_min_fibo_prio fib) r_routes;
					table.(e) <- Some(nr,nv,q,fib)
				|_ -> failwith "erreur5"
			end;
			tab_t.(i) <- suiv
	done;
	let dans_q = Array.make nb_routes false in
	let comp = ref 0 in
	let stop = ref false in
	while not (!stop) do
		(*print_int !comp;
		print_newline();
		comp:=!comp+1;*)
		let continu = ref true in
		while !continu do
			let (n_route,nb_voit,q,fib) = extraire_min_fibo r_routes in
			if est_vide_fibo fib then ()
			else begin
			match fib.minimum with
			|Vide -> failwith "erreur6"
			|Noeud e ->
				tps := e.prio;
				let _ = extraire_min_fibo fib in
				if est_vide_fibo fib && Queue.is_empty q then table.(n_route) <- None;
				match tab_t.(e.elem) with
				|[] -> if table.(n_route)<>None then begin
					ajoute_fibo (n_route,nb_voit-rep.(e.elem),q,fib) (obtenir_min_fibo_prio fib) r_routes;
					match table.(n_route) with
					|None -> failwith "erreur7"
					|Some (a,b,c,d) -> table.(n_route) <- Some (a,nb_voit - rep.(e.elem),c,d)
					end
					else if e.elem = trajet then begin
						for i = 0 to nb_routes - 1 do
							dans_q.(i) <- false;
						done;
						stop := true;
						continu := false
						end
				|i::suiv ->
					tab_t.(e.elem) <- suiv;
					let (a,b,_,_) = tab_routes.(i) in
					match mf.(a).(b) with
					|None -> failwith "erreur8"
					|Some f ->
						match table.(i) with
						|None ->
							let qq = Queue.create() in
							Queue.add e.elem qq;
							let v = (i,0,qq,cree_fibo_vide !utile) in
							ajoute_fibo v Float.infinity r_routes;
							table.(i) <- Some(v);
							dans_q.(i) <- true;
							if (nb_voit <> rep.(e.elem)) && (not (est_vide_fibo fib)) then begin
								ajoute_fibo (n_route,nb_voit - rep.(e.elem),q,fib) (obtenir_min_fibo_prio fib) r_routes;
								match table.(n_route) with
								|None -> failwith "erreur9"
								|Some (a,b,c,d) -> table.(n_route) <- Some (a,nb_voit - rep.(e.elem),c,d)
								end
							else table.(n_route) <- None
						|Some(v) ->
							let (r,voit,qq,fi) = v in
							let k = Hashtbl.find r_routes.table v in
							Hashtbl.remove r_routes.table v;
							Queue.add e.elem qq;
							table.(i) <- Some(v);
							Hashtbl.remove r_routes.table v;
							Hashtbl.add r_routes.table v k;
							dans_q.(i) <- true;
							if (nb_voit <> rep.(e.elem)) && (not (est_vide_fibo fib)) then begin
							ajoute_fibo (n_route,nb_voit - rep.(e.elem),q,fib) (obtenir_min_fibo_prio fib) r_routes;
								match table.(n_route) with
								|None -> failwith "erreur10"
								|Some (a,b,c,d) -> table.(n_route) <- Some (a,nb_voit - rep.(e.elem),c,d)
								end
							else table.(n_route) <- None
					end;
				if est_vide_fibo r_routes then continu := false
				else if obtenir_min_fibo_prio r_routes > !tps then continu := false
		done;
		for i = 0 to nb_routes - 1 do
			match dans_q.(i) with
			|false -> ()
			|true -> begin
				dans_q.(i) <- false;
				match table.(i) with
				|None -> failwith "erreur11"
				|Some e ->
					match Hashtbl.find r_routes.table e with
					|Vide -> failwith "erreur12"
					|Noeud n ->
						let (nr,nv,q,fib) = n.elem in
						let traj = ref (Queue.pop q) in
						let prem = !traj in
						let somme = ref (nv + rep.(!traj)) in
						Queue.add (!traj) q;
						while Queue.top q <> prem do
							traj := Queue.pop q;
							Queue.add (!traj) q;
							somme := !somme + rep.(!traj)
						done;
						supprime_element_fibo n.elem r_routes;
						let (a,b,_,_) = tab_routes.(nr) in
						match mf.(a).(b) with
						|None -> failwith "erreur13"
						|Some f ->
							while not (Queue.is_empty q) do
								ajoute_fibo (Queue.pop q) (!tps +. f !somme) fib
							done;
							let ppp = obtenir_min_fibo_prio fib in
							ajoute_fibo (nr,!somme,q,fib) ppp r_routes;
							begin
							match table.(nr) with
							|None -> failwith "erreur14"
							|Some (a,b,c,d) -> table.(nr) <- Some (a,!somme,c,d)
							end;
							Hashtbl.remove r_routes.table e;
							Hashtbl.add r_routes.table e (Noeud n);
			end;
		done;
		if est_vide_fibo r_routes then stop := true
	done;
	!tps
	

let debt (r:'a)(mf:'b)(tab_traj:'c)(tab_routes:'d) (trajet:'e) = 
	let nb_routes = Array.length tab_routes in
	let nb_traj = Array.length r in
	let tps = ref 0. in
	let init_route = Array.make nb_routes 0 in
	let tab_t = Array.copy tab_traj in
	let table = Array.make nb_routes None in
	let utile = ref 0 in
	let rep = Array.copy r in
	rep.(trajet) <- rep.(trajet) + 1;
	for i = 0 to nb_traj - 1 do
		if rep.(i) <> 0 then utile := !utile + 1;
		match tab_t.(i) with
		|[] -> failwith "erreur1"
		|e::suiv ->
			init_route.(e) <- init_route.(e) + rep.(i);	
	done;
	let r_routes : repartition_route_direct = cree_fibo_vide !utile in
	for i = 0 to nb_routes - 1 do
		let (d,a,_,_) = tab_routes.(i) in
		match init_route.(i),mf.(d).(a) with
		|0,_ -> ()
		|n,None -> failwith "erreur2"
		|n,Some(f) ->
			let v = (i,n,Queue.create(),cree_fibo_vide !utile) in
			ajoute_fibo v (f n) r_routes;
			table.(i) <- Some(v)
	done;
	for i = 0 to nb_traj - 1 do
		match rep.(i),tab_t.(i) with
		|0,_ -> tab_t.(i) <- []
		|_,[] -> failwith "erreur3"
		|n,e::suiv ->
			let (d,a,_,_) = tab_routes.(e) in
			begin
			match mf.(d).(a) with
			|None -> failwith "erreur4"
			|Some(f) ->
				match table.(e) with
				|Some(aa) ->
					let (nr,nv,q,fib) = aa in
					supprime_element_fibo aa r_routes;
					ajoute_fibo i (f init_route.(e)) fib;
					ajoute_fibo (nr,nv,q,fib) (obtenir_min_fibo_prio fib) r_routes;
					table.(e) <- Some(nr,nv,q,fib)
				|_ -> failwith "erreur5"
			end;
			tab_t.(i) <- suiv
	done;
	let dans_q = Array.make nb_routes false in
	let stop = ref false in
	tps,r_routes,table,dans_q,tab_t,tab_routes,mf,rep,nb_routes,!utile,stop,trajet



let midt (tps:'a) (r_routes:'b) (table:'c) (dans_q:'d) (tab_t:'e) (tab_routes:'f) (mf:'g) (rep:'h) (nb_routes:'i) (utile:'j) (stop:'k) (trajet:'l) =
	let continu = ref true in
	while !continu do
		let (n_route,nb_voit,q,fib) = extraire_min_fibo r_routes in
		if est_vide_fibo fib then ()
		else begin
		match fib.minimum with
		|Vide -> failwith "erreur6"
		|Noeud e ->
			tps := e.prio;
			let _ = extraire_min_fibo fib in
			if est_vide_fibo fib && Queue.is_empty q then table.(n_route) <- None;
			match tab_t.(e.elem) with
			|[] -> if table.(n_route)<>None then begin
				ajoute_fibo (n_route,nb_voit-rep.(e.elem),q,fib) (obtenir_min_fibo_prio fib) r_routes;
				match table.(n_route) with
				|None -> failwith "erreur7"
				|Some (a,b,c,d) -> table.(n_route) <- Some (a,nb_voit - rep.(e.elem),c,d)
				end
				else if e.elem = trajet then begin
					for i = 0 to nb_routes - 1 do
						dans_q.(i) <- false;
					done;
					stop := true;
					continu := false
					end
			|i::suiv ->
				tab_t.(e.elem) <- suiv;
				let (a,b,_,_) = tab_routes.(i) in
				match mf.(a).(b) with
				|None -> failwith "erreur8"
				|Some f ->
					match table.(i) with
					|None ->
						let qq = Queue.create() in
						Queue.add e.elem qq;
						let v = (i,0,qq,cree_fibo_vide utile) in
						ajoute_fibo v Float.infinity r_routes;
						table.(i) <- Some(v);
						dans_q.(i) <- true;
						if (nb_voit <> rep.(e.elem)) && (not (est_vide_fibo fib)) then begin
							ajoute_fibo (n_route,nb_voit - rep.(e.elem),q,fib) (obtenir_min_fibo_prio fib) r_routes;
							match table.(n_route) with
							|None -> failwith "erreur9"
							|Some (a,b,c,d) -> table.(n_route) <- Some (a,nb_voit - rep.(e.elem),c,d)
							end
						else table.(n_route) <- None
					|Some(v) ->
						let (r,voit,qq,fi) = v in
						let k = Hashtbl.find r_routes.table v in
						Hashtbl.remove r_routes.table v;
						Queue.add e.elem qq;
						table.(i) <- Some(v);
						Hashtbl.add r_routes.table v k;
						dans_q.(i) <- true;
						if (nb_voit <> rep.(e.elem)) && (not (est_vide_fibo fib)) then begin
						ajoute_fibo (n_route,nb_voit - rep.(e.elem),q,fib) (obtenir_min_fibo_prio fib) r_routes;
							match table.(n_route) with
							|None -> failwith "erreur10"
							|Some (a,b,c,d) -> table.(n_route) <- Some (a,nb_voit - rep.(e.elem),c,d)
							end
						else table.(n_route) <- None
				end;
			if est_vide_fibo r_routes then continu := false
			else if obtenir_min_fibo_prio r_routes > !tps then continu := false
	done;
	tps,r_routes,table,dans_q,tab_t,tab_routes,mf,rep,nb_routes,utile,stop,trajet


let fint (tps:'a) (r_routes:'b) (table:'c) (dans_q:'d) (tab_t:'e) (tab_routes:'f) (mf:'g) (rep:'h) (nb_routes:'i) (utile:'j) (stop:'k) (trajet:'l)=
	for i = 0 to nb_routes - 1 do
		match dans_q.(i) with
		|false -> ()
		|true -> begin
			dans_q.(i) <- false;
			match table.(i) with
			|None -> failwith "erreur11"
			|Some e ->
				match Hashtbl.find r_routes.table e with
				|Vide -> failwith "erreur12"
				|Noeud n ->
					let (nr,nv,q,fib) = n.elem in
					let traj = ref (Queue.pop q) in
					let prem = !traj in
					let somme = ref (nv + rep.(!traj)) in
					Queue.add (!traj) q;
					while Queue.top q <> prem do
						traj := Queue.pop q;
						Queue.add (!traj) q;
						somme := !somme + rep.(!traj)
					done;
					supprime_element_fibo n.elem r_routes;
					let (a,b,_,_) = tab_routes.(nr) in
					match mf.(a).(b) with
					|None -> failwith "erreur13"
					|Some f ->
						while not (Queue.is_empty q) do
							ajoute_fibo (Queue.pop q) (!tps +. f !somme) fib
						done;
						let ppp = obtenir_min_fibo_prio fib in
						ajoute_fibo (nr,!somme,q,fib) ppp r_routes;
						begin
						match table.(nr) with
						|None -> failwith "erreur14"
						|Some (a,b,c,d) -> table.(nr) <- Some (a,!somme,c,d)
						end;
						Hashtbl.remove r_routes.table e;
						Hashtbl.add r_routes.table e (Noeud n);
		end;
	done;
	if est_vide_fibo r_routes then stop := true;
	tps,r_routes,table,dans_q,tab_t,tab_routes,mf,rep,nb_routes,utile,stop,trajet




let at (tps:'a) (r_routes:'b) (table:'c) (dans_q:'d) (tab_t:'e) (tab_routes:'f) (mf:'g) (rep:'h) (nb_routes:'i) (utile:'j) (stop:'k) (trajet:'l) = 
	for i = 0 to 55 do
		let tps,r_routes,table,dans_q,tab_t,tab_routes,mf,rep,nb_routes,u,stop,trajet = midt tps r_routes table dans_q tab_t tab_routes mf rep nb_routes utile stop trajet in
		let tps,r_routes,table,dans_q,tab_t,tab_routes,mf,rep,nb_routes,u,stop,trajet = fint tps r_routes table dans_q tab_t tab_routes mf rep nb_routes u stop trajet in
		()
	done;
	tps,r_routes,table,dans_q,tab_t,tab_routes,mf,rep,nb_routes,utile,stop,trajet

(*
let liste_rep_ego (mf:mat_adj_fonction_temps)(tab_traj:tab_trajet)(tab_routes:tab_route)(voit_max:int) : repartition_trajet array=
	let nb_traj = Array.length tab_traj in
	let rep : repartition_trajet = Array.make nb_traj 0 in
	let tab_rep = Array.make (voit_max+1) (Array.copy rep) in
	if nb_traj <> 0 then
		begin
		for e = 1 to voit_max do (* Pour toutes les voitures *)
			print_int e;
			print_newline();
			let temps_min = ref (Float.infinity) in
			let trajet_min = ref(-1) in
			for i = 0 to nb_traj - 1 do (* Pour tous les trajets *)
				let tps = begin
					try temps_trajet rep mf tab_traj tab_routes i with
					|Probleme -> Float.infinity
				end in
				temps_min := Float.min (!temps_min) tps;
				if (!temps_min) = tps then trajet_min := i
			done;
			rep.(!trajet_min) <- rep.(!trajet_min) + 1;
			tab_rep.(e) <- Array.copy rep;
		done;
		end;
	tab_rep
*)

let liste_rep_ego (mf:mat_adj_fonction_temps)(tab_traj:tab_trajet)(tab_routes:tab_route)(voit_max:int) : repartition_trajet array=
	let nb_traj = Array.length tab_traj in
	let rep : repartition_trajet = Array.make nb_traj 0 in
	let mini = Array.make nb_traj 0. in
	for i = 0 to nb_traj - 1 do
		rep.(i) <- 1;
		mini.(i) <- temps rep mf tab_traj tab_routes 1;
		rep.(i) <- 0;
	done;
	let repo = Array.copy rep in
	let tab_rep = Array.make (voit_max+1) (Array.copy rep) in
	if nb_traj <> 0 then
		begin
		for e = 1 to voit_max do (* Pour toutes les voitures *)
			(*print_int e;
			print_newline();*)
			let temps_min = ref (Float.infinity) in
			let trajet_min = ref(-1) in
			for i = 0 to nb_traj - 1 do (* Pour tous les trajets *)
				if !temps_min >= mini.(i) then begin
					let tps = begin
						try temps_trajet rep mf tab_traj tab_routes i with
						|Probleme -> Float.infinity
					end in
					temps_min := Float.min (!temps_min) tps;
					if (!temps_min) = tps then trajet_min := i;
					if tps <> Float.infinity then mini.(i) <- tps;
				end;
			done;
			rep.(!trajet_min) <- rep.(!trajet_min) + 1;
			tab_rep.(e) <- Array.copy rep;
		done;
		end;
	tab_rep

(*
let liste_rep_social (mf:mat_adj_fonction_temps)(tab_trajets:tab_trajet)(tab_routes:tab_route)(voit_max:int) : repartition_trajet array=
	let nb_trajets = Array.length tab_trajets in
	let rep : repartition_trajet = Array.make nb_trajets 0 in
	let tab_rep = Array.make (voit_max+1) (Array.copy rep) in
		if nb_trajets <> 0 then
		begin
		for e = 1 to voit_max do (* Pour toutes les voitures *)
			print_int e;
			print_newline();
			let temps_min = ref (Float.infinity) in
			let trajet_min = ref(-1) in
			for i = 0 to nb_trajets - 1 do (* Pour tous les trajets *)
				let rep_bis = Array.copy rep in
				rep_bis.(i) <- rep_bis.(i) + 1;
				let tps =
					begin
					try temps rep_bis mf tab_trajets tab_routes with
					|Probleme -> Float.infinity
				end in
				temps_min := Float.min (!temps_min) tps;
				if (!temps_min) = tps then trajet_min := i
			done;
			rep.(!trajet_min) <- rep.(!trajet_min) + 1;
			tab_rep.(e) <- Array.copy rep; 
		done;
		end;
	tab_rep
*)


let liste_rep_social (mf:mat_adj_fonction_temps)(tab_traj:tab_trajet)(tab_routes:tab_route)(voit_max:int): repartition_trajet array=
	let nb_traj = Array.length tab_traj in
	let rep : repartition_trajet = Array.make nb_traj 0 in
	let mini = Array.make nb_traj 0. in
	for i = 0 to nb_traj - 1 do
		rep.(i) <- 1;
		mini.(i) <- temps rep mf tab_traj tab_routes 1;
		rep.(i) <- 0;
	done;
	let min = Array.copy mini in
	let tab_rep = Array.make (voit_max+1) (Array.copy rep) in
		if nb_traj <> 0 then
		begin
		let temps_min = ref (Float.infinity) in
		let trajet_min = ref(0) in
		for e = 1 to voit_max do (* Pour toutes les voitures *)
			(*print_int e;
			print_newline();*)
			let repo = Array.copy rep in
			repo.(!trajet_min) <- repo.(!trajet_min) + 1;
			temps_min := begin
				try temps repo mf tab_traj tab_routes e with
				|Probleme -> Float.infinity
			end;
			for i = 0 to nb_traj - 1 do (* Pour tous les trajets *)
				if !temps_min >= min.(i) then begin
					let rep_bis = Array.copy rep in
					rep_bis.(i) <- rep_bis.(i) + 1;
					let tps =
						begin
						try temps rep_bis mf tab_traj tab_routes e with
						|Probleme -> Float.infinity
					end in
					temps_min := Float.min (!temps_min) tps;
					if (!temps_min) = tps then trajet_min := i
				end;
			done;
			for i = 0 to nb_traj - 1 do
				min.(i) <- (!temps_min *. (float_of_int(e)) +. mini.(i)) /. (float_of_int(e+1));
			done;
			rep.(!trajet_min) <- rep.(!trajet_min) + 1;
			tab_rep.(e) <- Array.copy rep; 
		done;
		end;
	tab_rep





let liste_prix_anarchie (rep_e:repartition_trajet array) (rep_s:repartition_trajet array)(mf:mat_adj_fonction_temps)(tab_traj:tab_trajet)(tab_routes:tab_route) : (int*float) array =
	let t = Array.make (Array.length rep_e) (0,0.)  in
	for i = 0 to (Array.length rep_e) - 1 do
		let temps_e = try temps rep_e.(i) mf tab_traj tab_routes i with
		|Probleme -> 1. in
		let temps_s = try temps rep_s.(i) mf tab_traj tab_routes i with
		|Probleme -> 1. in
		t.(i) <- (i,temps_e/.temps_s)
	done;
	t


(*
let liste_diff_prix_anarchie_sans_i (r_route:list_routes)(voit_max:int)(i:int) : (int*float) list =
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



let prix_anarchie_max (lpa:(int*float) array) : (int*float) =
	let max = ref 0. in
	let max_nb = ref 0 in
	for i = 1 to (Array.length lpa)-1 do
		let (nb,t) = lpa.(i) in
		if t > !max
			then begin max:=t; max_nb:=nb end
		else ()
	done;
	(!max_nb,!max)




    
let prix_anarchie_moy (lpa:(int*float) array) : float =
	let som = ref 0. in
	for i = 1 to (Array.length lpa) - 1 do
		let (_,t) = lpa.(i) in
			som := !som +. t
	done;
	!som /. float_of_int(Array.length lpa - 1)



let prix_anarchie_modif (lpa:(int*float) array) : int =
	let nb_min = ref (Array.length lpa) in
	for i = 1 to (Array.length lpa)-1 do
		let (v,t) = lpa.(i) in
		if t<>1. && v < !nb_min then nb_min := v
	done;
	!nb_min



let mini (mf:mat_adj_fonction_temps)(tab_trajets:tab_trajet)(tab_routes:tab_route) =
	let nb_trajets = Array.length tab_trajets in
	let rep : repartition_trajet = Array.make nb_trajets 0 in
	let min = Array.make nb_trajets (0.,0) in
	for i = 0 to nb_trajets - 1 do
		rep.(i) <- 1;
		min.(i) <- (temps rep mf tab_trajets tab_routes 1,i);
		rep.(i) <- 0;
	done;
	Array.sort compare min;
	min


let coupe_tab (tab:(float*int) array)(n:int) = 
	let t = Array.make n 0 in
	for i = 0 to n-1 do
		let _,j = tab.(i) in
		t.(i) <- j;
	done;
	t



(*34 - 31 - 1750 voit*)

let dep = 34
let arr = 31
let voit_tot = 1750
let g = graphe_trajet tab_routes
let mf = mat_temps_route g tab_routes
let t_traj = tab_trajet g dep arr tab_routes
let m_traj = coupe_tab (mini mf t_traj tab_routes) 50
let tab_traj = Array.init 50 (fun x -> t_traj.((m_traj).(x)))
let ltre = liste_rep_ego mf tab_traj tab_routes voit_tot
let ltrs = liste_rep_social mf tab_traj tab_routes voit_tot
let lpa = liste_prix_anarchie ltre ltrs mf tab_traj tab_routes

let trsi (i:int) = 
	let t = Array.make (Array.length tab_routes - 1) (0,0,0.,0) in
	for j = 0 to Array.length tab_routes - 1 do
		if j < i then t.(j) <- tab_routes.(j)
		else if j = i then ()
		else t.(j-1) <- tab_routes.(j)
	done;
	t





let i_min (n:int)(v:int) =
	let min_t = ref Float.infinity in
	let min_i = ref (-1) in 
	for i = 0 to n do
		let tab_routes_sans_i = trsi i in
		let g_sans_i = graphe_trajet tab_routes_sans_i in
		let t_traj_sans_i = tab_trajet g_sans_i dep arr tab_routes_sans_i in
		let m_traj_sans_i = coupe_tab (mini mf t_traj_sans_i tab_routes_sans_i) 50 in
		let tab_traj_sans_i = Array.init 50 (fun x -> t_traj_sans_i.((m_traj_sans_i).(x))) in
		let ltre_sans_i = liste_rep_ego mf tab_traj_sans_i tab_routes_sans_i v in
		let t = temps ltre_sans_i.(v) mf tab_traj_sans_i tab_routes_sans_i v in
		print_int i;
		print_string " : ";
		print_float t;
		print_newline();
		if t < !min_t then begin
			min_t:=t;
			min_i:=i;
	end
	done;
	!min_i,(!min_t-.(temps ltre.(v) mf tab_traj tab_routes v))


let i_min2 (n:int)(v:int) = 
	let tab_routes_sans_i = trsi n in
	let g_sans_i = graphe_trajet tab_routes_sans_i in
	let t_traj_sans_i = tab_trajet g_sans_i dep arr tab_routes_sans_i in
	let m_traj_sans_i = coupe_tab (mini mf t_traj_sans_i tab_routes_sans_i) 50 in
	let tab_traj_sans_i = Array.init 50 (fun x -> t_traj_sans_i.((m_traj_sans_i).(x))) in
	let ltre_sans_i = liste_rep_ego mf tab_traj_sans_i tab_routes_sans_i v in
	(*let ltrs_sans_i = liste_rep_social mf tab_traj_sans_i tab_routes_sans_i v in
	*)ltre_sans_i,tab_traj_sans_i(*,ltrs_sans_i*)
		
	
	
let ltt (r:repartition_trajet array)(mf:mat_adj_fonction_temps)(tab_traj:tab_trajet)(tab_routes:tab_route)  =
	let t = Array.make (Array.length r) (0,0.) in
	for i = 0 to Array.length r - 1 do
		t.(i) <- i,temps r.(i) mf tab_traj tab_routes i;
	done;
	t

	(*
	let ltrs_sans_i = liste_rep_social mf_sans_i tab_traj_sans_i tab_routes_sans_i nb_voit
	let lpa_sans_i = liste_prix_anarchie ltre_sans_i ltrs_sans_i mf_sans_i tab_traj_sans_i tab_routes_sans_i
	let (pama_v_sans_i,pama_t_sans_i) = prix_anarchie_max lpa_sans_i
	let pamo_sans_i = prix_anarchie_moy lpa_sans_i
	*)


let graphique (lpa:(int*float) array) =

let (pama_v,pama_t) = prix_anarchie_max lpa in
let pamo = prix_anarchie_moy lpa in

let padiff = prix_anarchie_modif lpa in
let ec = if (padiff<300) then 0 else padiff/2 in
let padiff2 = if ec = 0 then 0 else padiff in

let u = Graphics.open_graph "" in
u;
let mv x y = Graphics.moveto x y in
let mv2 x y = Graphics.lineto x y in

let f1 x =
    mv 90 (150+x*150);
    mv2 110 (150+x*150) in

let f2 x =
    mv (200+x*100) 110;
    mv2 (200+x*100) 90 in


let calc_x a =
    int_of_float(100. +. (float_of_int(a-padiff2+ec)/.float_of_int(voit_tot-padiff2+ec))*.1200.) in
    
let calc_y t =
    int_of_float(150. +. (t-.1.)*.(600.)/.(pama_t-.1.)) in
	
let x_max = (calc_x pama_v) in
let y_max = (calc_y pama_t) + 20 in



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
        Graphics.draw_string(string_of_int(((voit_tot-padiff2+ec)/12)*(j+1)+(padiff2-ec)));
    done;
    mv 95 75;
    Graphics.draw_string(string_of_int(padiff2-ec));
    mv 1300 50;
    Graphics.draw_string("Nombre de voitures");
    mv 65 830;
    Graphics.draw_string("Prix de l'anarchie");
    mv 100 150;
    Graphics.set_color 180 in
    
param;

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
	Graphics.draw_string(" voitures.") in

flechemax;

let param2 =
	mv 100 150;
	let lpa_der = ref 1. in
	for i = (padiff2-ec) to (voit_tot-1) do
		let (v,tt) = lpa.(i) in
		let t = if tt < 1. then !lpa_der else begin lpa_der:=tt; tt end in
		if (i mod ((voit_tot-padiff2+ec)/240 + 1) = 0) then
			mv2 (calc_x v) (calc_y (t));
		if (i mod ((voit_tot-padiff2+ec)/240 + 1) = 0) then begin
			Graphics.set_color 255;
			Graphics.fill_circle (calc_x v) (calc_y (t)) 5;
			Graphics.set_color 180;
			end
	done in

param2;

	()



let graphique2 (lt1:(int*float) array) (lt2:(int*float) array) = 
	let n_v = Array.length lt1 in
	let ec =  0 in 
	let padiff2 = 0 in 
	let a = 
		let aa = Array.make (Array.length lt1) 0. in
		for i = 0 to (n_v-1) do
			let (v,tt) = lt1.(i) in
			let (_,tt2) = lt2.(i) in
			aa.(i) <- tt/.tt2;
		done;
		aa in
	
let tmax = 
	let aa = a in
	let m = ref Float.neg_infinity in
	for i = 0 to n_v - 1 do
		m:=max aa.(i) !m;
	done;
	!m
	in



	let u = Graphics.open_graph "" in
	u;
	let mv x y = Graphics.moveto x y in
	let mv2 x y = Graphics.lineto x y in

	let f1 x =
		  mv 90 (150+x*150);
		  mv2 110 (150+x*150) in

	let f2 x =
		  mv (200+x*100) 110;
		  mv2 (200+x*100) 90 in

	let calc_x a =
		  int_of_float(100. +. (float_of_int(a)/.float_of_int(n_v))*.1200.) in
		  
	let calc_y t =
		  int_of_float(150. +. (t)*.(600.)/.(tmax)) in
		
	let x_max = (calc_x n_v) in
	let y_max = (calc_y tmax) + 20 in



	let param =
		  Graphics.set_window_title "Écart du temps égoiste entre deux configurations du réeaux";
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
		      Graphics.draw_string(string_of_float(((((Float.round(tmax*.100.))/.100.))/.4.)*.float_of_int(i)));
		  done;
		  for j=0 to 11 do
		      f2 j;
		      mv (Graphics.current_x ()-5) (Graphics.current_y ()-20);
		      Graphics.draw_string(string_of_int(((n_v-padiff2+ec)/12)*(j+1)+(padiff2-ec)));
		  done;
		  mv 95 75;
		  Graphics.draw_string(string_of_int(padiff2-ec));
		  mv 1300 50;
		  Graphics.draw_string("Nombre de voitures");
		  mv 65 830;
		  Graphics.draw_string("Rapport temps trajets");
		  mv 100 150;
		  Graphics.set_color 180 in
		
	param;

	let param2 =
		let aa = a in
		mv (calc_x 1) (calc_y aa.(1));
		for i = 1 to (n_v-1) do	
			if (i mod (n_v/240 + 1) = 0) then
				mv2 (calc_x i) (calc_y aa.(i));
			if (i mod ((n_v-padiff2+ec)/240 + 1) = 0) then begin
				Graphics.set_color 255;
				Graphics.fill_circle (calc_x i) (calc_y aa.(i)) 5;
				Graphics.set_color 180;
				end
		done in
	Graphics.set_color Graphics.black;
	mv (calc_x 0) (calc_y 1.);
	mv2 (calc_x 1750) (calc_y 1.);

	param2;
		()









