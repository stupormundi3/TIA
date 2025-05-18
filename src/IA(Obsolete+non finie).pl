%% etat=(Liste_lutins,Liste_des_ponds_suprimer)
%% ou lutin=(X,Y,team)
%% et liste des ponds suprimer car par default plaeaut est plein et joueur vont retirer des ponts
%% ou faire des rotation = ajouter le pont qui rotate dans la liste des suprimé et retirer le "nouveau" pont créé par la rotation de la liste
%% un pond = ((X1,Y1),(X2,Y2)) paire de coordonée ou X1<=X2 et Y1<=Y2 pour eviter confusion entre pond ça ou avoir f is equivalent
%%qui check si 2 ponds font la meme chose (ex ((1,2),(1,3)) et ((1,3),(1,2))
check_pos_lutin((X,Y)):- X>=0, X=<5, Y>=0, Y=<5.
%%equivalent((X1,Y1),(X2,Y2)).

%% check si pond valide 
check_pond(((X,Y),(X2,Y))):-(X=X2-1), check_pos_lutin((X,Y)),check_pos_lutin(X2,Y).
check_pond(((X,Y),(X,Y2))):-(Y=Y2-1), check_pos_lutin((X,Y)),check_pos_lutin(X,Y2).

init([],[]).

place((X,Y,Team),(Lutins,Ponds),(Lutins_mod,Ponds)):- check_pos_lutin5(X,Y),\+member(Lutins,(X2,Y2,Team)),member(Lutins_mod,(X,Y,Team)).

%% en princpe gere toute les directions (non fix)
move((X,Y,Team),(X2,Y2,Team),(Lutins,Ponds),(Lutins_mod,Ponds)):-member((X,Y,Team),Lutins),%%check que lutin existe
check_pond(((X,Y),(X2,Y2))),%%check que delpacement est valide
(\+ member(((X,Y),(X2,Y2)),Ponds),\+ member((X2,Y2,_),Lutins)) %% check que pond permetant le deplacement existe
;(\+ member(((X2,Y2),(X,Y)),Ponds), %% check que pond permetant le deplacement existe (dans lautre sens car ponds sont one way)
\+ member((X2,Y2,_),Lutins)),%%check que position est dispo
delete(Lutins,(X,Y,Team),L), %% L = lutins sans le lutin qu'on bouge
append(L,(X2,Y2,Team),Lutin_mod). %% fait que lutin mod = L+nouvelle position du lutin


%%retirerpond((Lutins,Ponds),Pond,(Lutins,Ponds_mod)):- \+ member(Pond,Ponds),append(Ponds,Pond,Ponds_mod).
%%pour droite et haut = base reste la meme mais destination change
%% besoin changement que 90° pour rotation donc peut etre 4 transitions a la place doit rework le code
%%rotate_pond((Lutins,Ponds),((Xp,Yp),(Xp2,Yp2)),((Xp,Yp),(Xp2_mod,Yp2_mod)),Ponds_mod()):-check_pond(((Xp,Yp),(Xp2,Yp2))),%%check que la position de base du pond est valide
%%\+ member(((Xp,Yp),(Xp2,Yp2)),Ponds),%% check que le pond existe
%%member(((Xp,Yp),(Xp2_mod,Yp2_mod)),Ponds),%% check que place de rotation est libre
%%apend(Ponds,((Xp,Yp),(Xp2,Yp2)),X),%% X= ponds avec anciene position de pond ajouter
%%check_pond((Xp,Yp),(Xp2_mod,Yp2_mod)),%% check nouvelle position est valide
%%delete(X,((Xp,Yp),(Xp2_mod,Yp2_mod)),Ponds_mod).%% pont mod = X ou retire la nouvelle position du pond 

%% rotate pond
%%rotate_pond((Lutins,Ponds),((Xp,Yp),(Xp2,Yp2)),((Xp_mod,Yp_mod),(Xp2,Yp2)),Ponds_mod()):-check_pond(((Xp,Yp),(Xp2,Yp2))),%%check que la position de base du pond est valide
%%\+ member(Ponds,((Xp,Yp),(Xp2,Yp2))),%% check que le pond existe

%%member(((Xp_mod,Yp_mod),(Xp2,Yp2)),Ponds),%% check que place de rotation est libre
%%apend(Ponds,((Xp,Yp),(Xp2,Yp2)),X),%% X= ponds avec anciene position de pond ajouter
%%check_pond((Xp_mod,Yp_mod),(Xp2,Yp2)),%% check nouvelle position est valide
%d%elete(X,((Xp_mod,Yp_mod),(Xp2,Yp2)),Ponds_mod).%% pont mod = X ou retire la nouvelle position du pond 
%% pour quand tour d'un joueur se fini devra check lutins et si une team est éliminer les suprimer de la liste des lutin 

rotate_pond((Lutins,Ponds),((Xp,Yp),(Xp2,Yp2)),((Xp_mod,Yp_mod),(Xp2_mod,Yp2_mod)),(Lutins,Ponds_mod)):-rotate_right((Lutins,Ponds),((Xp,Yp),(Xp2,Yp2)),((Xp_mod,Yp_mod),(Xp2_mod,Yp2_mod)),Ponds_mod);rotate_left((Lutins,Ponds),((Xp,Yp),(Xp2,Yp2)),((Xp_mod,Yp_mod),(Xp2_mod,Yp2_mod)),Ponds_mod).
%%gauche-> haut
rotate_right((Ponds),((Xp,Yp),(Xp2,Yp)),((Xp2,Yp),(Xp2,Yp_mod)),Ponds_mod):-Yp_mod is Yp+1,
check_pond(((Xp,Yp),(Xp2,Yp))),%%verifie pond valide
\+member(((Xp,Yp),(Xp2,Yp)),Ponds),%% check que le pond existe
member(((Xp2,Yp),(Xp2,Yp_mod)),Ponds), %% verifie destination libre
apend(Ponds,((Xp,Yp),(Xp2,Yp)),X),%% X= ponds avec anciene position de pond ajouter a list des ponts suprimer
check_pond((Xp2,Yp),(Xp2,Yp_mod)),%% check nouvelle position est valide (non néscessaire sauf si ponds a des problemes/)
delete(X,((Xp2,Yp),(Xp2,Yp_mod)),Ponds_mod).%% pont mod = X ou retire la nouvelle position du pond 

%% haut -> droite
rotate_right((Ponds),((Xp,Yp),(Xp,Yp2)),((Xp2,Yp),(Xp2_mod,Yp)),Ponds_mod):-Yp2 is Yp+1,Xp2_mod is Xp2+1,
check_pond(((Xp,Yp),(Xp,Yp2))),%%verifie pond valide,
\+member(((Xp,Yp),(Xp,Yp2)),Ponds),%% check que le pond existe
member(((Xp2,Yp),(Xp2_mod,Yp)),Ponds), %% verifie destination libre
apend(Ponds,((Xp,Yp),(Xp,Yp2)),X),%% X= ponds avec anciene position de pond ajouter a list des ponts suprimer
check_pond((Xp2,Yp),(Xp2_mod,Yp)),%% check nouvelle position est valide (non néscessaire sauf si ponds a des problemes/)
delete(X,((Xp2,Yp),(Xp2_mod,Yp)),Ponds_mod).%% pont mod = X ou retire la nouvelle position du pond 

%% droite -> bas
rotate_right((Ponds),((Xp,Yp),(Xp2,Yp)),((Xp,Yp_mod),(Xp,Yp)),Ponds_mod):-Yp_mod is Yp-1,
check_pond(((Xp,Yp),(Xp2,Yp))),%%verifie pond valide,
\+member(((Xp,Yp),(Xp2,Yp)),Ponds),%% check que le pond existe
member(((Xp,Yp_mod),(Xp,Yp)),Ponds), %% verifie destination libre
apend(Ponds,((Xp,Yp),(Xp2,Yp)),X),%% X= ponds avec anciene position de pond ajouter a list des ponts suprimer
check_pond((Xp,Yp_mod),(Xp,Yp)),%% check nouvelle position est valide (non néscessaire sauf si ponds a des problemes/)
delete(X,((Xp,Yp_mod),(Xp,Yp)),Ponds_mod).%% pont mod = X ou retire la nouvelle position du pond 

%% bas -> gauche
rotate_right((Ponds),((Xp,Yp),(Xp,Yp2)),((Xp_mod,Yp),(Xp,Yp)),Ponds_mod):-Xp_mod is Xp-1,
check_pond(((Xp,Yp),(Xp,Yp2))),%%verifie pond valide,
\+member(((Xp,Yp),(Xp,Yp2)),Ponds),%% check que le pond existe
member(((Xp_mod,Yp),(Xp,Yp)),Ponds), %% verifie destination libre
apend(Ponds,((Xp,Yp),(Xp,Yp2)),X),%% X= ponds avec anciene position de pond ajouter a list des ponts suprimer
check_pond((Xp_mod,Yp),(Xp,Yp)),%% check nouvelle position est valide (non néscessaire sauf si ponds a des problemes/)
delete(X,((Xp_mod,Yp),(Xp,Yp)),Ponds_mod).%% pont mod = X ou retire la nouvelle position du pond 

%% ici vas contre horloge fait a base de copy paste donc chance d'ereur

%% gauche -> bas
rotate_right((Ponds),((Xp_mod,Yp),(Xp,Yp)),((Xp,Yp),(Xp,Yp2)),Ponds_mod):-Xp_mod is Xp-1,
check_pond(((Xp_mod,Yp),(Xp,Yp))),%%verifie pond valide,
\+member(((Xp_mod,Yp),(Xp,Yp)),Ponds),%% check que le pond existe
member(((Xp,Yp),(Xp,Yp2)),Ponds), %% verifie destination libre
apend(Ponds,((Xp_mod,Yp),(Xp,Yp)),X),%% X= ponds avec anciene position de pond ajouter a list des ponts suprimer
check_pond((Xp,Yp),(Xp,Yp2)),%% check nouvelle position est valide (non néscessaire sauf si ponds a des problemes/)
delete(X,((Xp,Yp),(Xp,Yp2)),Ponds_mod).%% pont mod = X ou retire la nouvelle position du pond 

%% bas-> droite
rotate_right((Ponds),((Xp,Yp_mod),(Xp,Yp)),((Xp,Yp),(Xp2,Yp)),Ponds_mod):-Yp is Yp_mod+1,
check_pond(((Xp,Yp_mod),(Xp,Yp))),%%verifie pond valide,
\+member(((Xp,Yp_mod),(Xp,Yp)),Ponds),%% check que le pond existe
member(((Xp,Yp),(Xp2,Yp)),Ponds), %% verifie destination libre
apend(Ponds,((Xp,Yp_mod),(Xp,Yp)),X),%% X= ponds avec anciene position de pond ajouter a list des ponts suprimer
check_pond((Xp,Yp),(Xp2,Yp)),%% check nouvelle position est valide (non néscessaire sauf si ponds a des problemes/)
delete(X,((Xp,Yp),(Xp2,Yp)),Ponds_mod).%% pont mod = X ou retire la nouvelle position du pond 

%% droite -> haut
%%(Xp,Yp),(Xp,Yp2) artefact ici au cas ou
rotate_right((Ponds),((Xp2,Yp),(Xp2_mod,Yp)),((Xp,Yp),(Xp,Yp2)),Ponds_mod):-Yp is Yp2-1,Xp2 is Xp2_mod-1,
check_pond(((Xp2,Yp),(Xp2_mod,Yp))),%%verifie pond valide,
\+member(((Xp2,Yp),(Xp2_mod,Yp)),Ponds),%% check que le pond existe
member(((Xp,Yp),(Xp,Yp2)),Ponds), %% verifie destination libre
apend(Ponds,((Xp2,Yp),(Xp2_mod,Yp)),X),%% X= ponds avec anciene position de pond ajouter a list des ponts suprimer
check_pond((Xp,Yp),(Xp,Yp2)),%% check nouvelle position est valide (non néscessaire sauf si ponds a des problemes/)
delete(X,((Xp,Yp),(Xp,Yp2)),Ponds_mod).%% pont mod = X ou retire la nouvelle position du pond 

%% haut -> gauche
%%(Xp,Yp),(Xp2,Yp) artefact ici au cas ou
rotate_right((Ponds),((Xp2,Yp),(Xp2,Yp_mod)),((Xp,Yp),(Xp2,Yp)),Ponds_mod):-Yp is Yp2_mod-1,
check_pond(((Xp2,Yp),(Xp2,Yp_mod))),%%verifie pond valide,
\+member(((Xp2,Yp),(Xp2,Yp_mod)),Ponds),%% check que le pond existe
member(((Xp,Yp),(Xp2,Yp)),Ponds), %% verifie destination libre
apend(Ponds,((Xp2,Yp),(Xp2,Yp_mod)),X),%% X= ponds avec anciene position de pond ajouter a list des ponts suprimer
check_pond((Xp,Yp),(Xp2,Yp)),%% check nouvelle position est valide (non néscessaire sauf si ponds a des problemes/)
delete(X,((Xp,Yp),(Xp2,Yp)),Ponds_mod).%% pont mod = X ou retire la nouvelle position du pond 


min_max(State,Depth,true,Bestmove,Bestval):-
    findall(Val-Move,%%syntaxe pour tuple ou 1er el - deuxiememe  (aussi en liste ?)
    (posible_move(Move),
    play(Move,State,Newstate),
    min_max(Newstate,Depth+1,false,Bestmove,Bestval))
    ,Moves),
    max_member(Bestval-Bestmove,Moves).

min_max(State,Depth,false,Bestmove,Bestval):-
    findall(Val-Move,
    (posible_move(Move),
    play(Move,State,Newstate),
    min_max(Newstate,Depth+1,true,Bestmove,Bestval))
    ,Moves),
    min_member(Bestval-Bestmove,Moves).
    
eliminate((Lutins,Ponds),(Lutin_x,Lutin_y,Lutin_team),Lutins_mod):-member((Lutin_x,Lutin_y,Lutin_team),Lutins),%% check lutin existe
\+ move((Lutin_x,Lutin_y,Lutin_team),(X2,Y2,Team),(Lutins,Ponds),(Lutins_moove,Ponds)),%% check que peut pas boujer
delete(Lutins,(Lutin_x,Lutin_y,Lutin_team),Lutin_mod),!.%% delete puis cut pour que essaye pas autre option.

eliminate((Lutins,Ponds),(Lutin_x,Lutin_y,Lutin_team),Lutins).%%member((Lutin_x,Lutin_y,Lutin_team),Lutins)).%% vas juste etre la pour renvoyer true sans changer liste si lutin ne peut etre eliminer

delete_pond(Ponds,Pond,Ponds_mod):-check_pond(Pond),delete(Ponds,Pond,Ponds_mod).

%%a very basic and unoptimised turn
valid_turn_basic((Lutins,Ponds),Team,(Lutins_mod,Ponds_mod)):-move(Lutins,(X,Y,Team),(X2,Y2,Team),(Lutins,Pond),(Lutin_mod_1,Ponds)),%%fait tout les movements posibles
%%ici vas tourner pond puis pour chaque case afectée checker si un lutin imobiliser est dessud et si oui l'elimine
((rotate_pond((Lutins,Ponds),((Xp,Yp),(Xp2,Yp2)),((Xp_mod,Yp_mod),(Xp2_mod,Yp2_mod)),(Lutins,Ponds_mod)),eliminate((Lutin_mod_1,Ponds_mod),(Xp,Yp,_),Lutin_mod_2),eliminate((Lutin_mod_2,Ponds_mod),(Xp2,Yp2,_),Lutin_mod_3),eliminate((Lutin_mod_3,Ponds_mod),(Xp_mod,Yp_mod,_),Lutin_mod_4),eliminate((Lutin_mod_4,Ponds_mod),(Xp2_mod,Yp2_mod,_),Lutin_mod));
%% ici meme mais doit juste verifier les case touchées par le pond suprimé.
(delete_pond(Ponds,((X,Y),(X2,Y2)),Ponds_mod),eliminate((Lutin_mod_1,Ponds_mod),(X,Y,_),Lutin_mod_2),eliminate((Lutin_mod_2),Ponds_mod,(X2,Y2,_),Lutin_mod))).

%% avoid edge reduit a un plateau 4x4 du coup et ddiminue coups
movement_heuristic_1:-((Ponds,Lutins),Team,(Lutin_mod,Ponds_mod)):-()
tree_node(value,min_or_max,game_state,depth).
%%if depht = 0, heuristique (game state,value),
%% else faire enfants (jouer selon heuristique) et update en direct selon ligne suivante
%% value = if min_or_max, min(val enfant ,value)) else value =max (valeur enfant,enfant value)
%% if min and value.parent > value cut car aplpha beta elsif max and parent.value < value cut car alpha beta

