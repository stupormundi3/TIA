%% etat=(Liste_lutins,Liste_des_ponds_suprimer)
%% ou lutin=(X,Y,team)
%% et liste des ponds suprimer car par default plaeaut est plein et joueur vont retirer des ponts
%% ou faire des rotation = ajouter le pont qui rotate dans la liste des suprimé et retirer le "nouveau" pont créé par la rotation de la liste
%% un pond = ((X1,Y1),(X2,Y2)) paire de coordonée ou X1<=X2 et Y1<=Y2 pour eviter confusion entre pond ça ou avoir f is equivalent
%%qui check si 2 ponds font la meme chose (ex ((1,2),(1,3)) et ((1,3),(1,2))
check_pos_lutin((X,Y)):-0<=X<=4, 0<=Y<=4.
equivalent((X1,Y1),(X2,Y2))

%% check si pond valide 
check_pond(((X,Y),(X2,Y))):-(X=X2-1), check_pos_lutin((X,Y)),check_pos_lutin(X2,Y).
check_pond(((X,Y),(X,Y2))):-(Y=Y2-1), check_pos_lutin((X,Y)),check_pos_lutin(X,Y2).

init([],[])
place((X,Y,Team),(Lutins,Ponds),(Lutins_mod,Ponds)) check_pos_lutin5(X,Y),\+member(Lutins,(X2,Y2)),member(Lutins_mod,(X,Y,Team)).
%% en princpe gere toute les directions
move((X,Y,Team),(X2,Y2,Team),(Lutins,Ponds),(Lutins_mod,pond)):-member((X,Y,Team),Lutins),%%check que lutin existe
check_pond(((X,Y),(X2,Y2))),%%check que delpacement est valide
\+ member(Ponds,((X,Y),(X,Y))), %% check que pond permetant le deplacement existe
\+ member(Lutin,(X2,Y2,_),%%check que position est dispo
delete(Lutins,(X,Y,Team),L), %% L = lutins sans le lutin qu'on bouge
append(L,(X2,Y2,Team),Lutin_mod). %% fait que lutin mod = L+nouvelle position du lutin


retirerpond((Lutins,Ponds),Pond,(Lutins,Ponds_mod)):- \+ member(Ponds,Pond),append(Ponds,Pond,Ponds_mod).
%%pour droite et haut = base reste la meme mais destination change

rotate_pond((Lutins,Ponds),((Xp,Yp),(Xp2,Yp2)),((Xp,Yp),(Xp2_mod,Yp2_mod)),Ponds_mod()):-check_pond(((Xp,Yp),(Xp2,Yp2))),%%check que la position de base du pond est valide
\+ member(Ponds,Pond),%% check que le pond existe
member(((Xp,Yp),(Xp2_mod,Yp2_mod)),Ponds),%% check que place de rotation est libre
apend(Ponds,Pond,X),%% X= ponds avec anciene position de pond ajouter
check_pond((Xp,Yp),(Xp2_mod,Yp2_mod)),%% check nouvelle position est valide
delete(X,((Xp,Yp),(Xp2_mod,Yp2_mod)),Ponds_mod).%% pont mod = X ou retire la nouvelle position du pond 

%% pour quand tour d'un joueur se fini devra check lutins et si une team est éliminer les suprimer de la liste des lutin 
eliminate()