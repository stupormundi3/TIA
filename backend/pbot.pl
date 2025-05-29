% -----------------------------------------------------------------------------
% Module PBot : bot explicateur pour PontuXL
% expose le prédicat produire_reponse/2
% -----------------------------------------------------------------------------
:- module(pbot, [produire_reponse/2]).

:- use_module(library(lists)).
:- use_module(library(charsio)).  % pour string_codes/2
:- use_module(library(pcre)).     % pour la recherche de sous-chaînes

% -----------------------------------------------------------------------------
% Base de connaissances : réponses statiques
% -----------------------------------------------------------------------------
db_response(commence,
    "Par convention, c est au joueur en charge des lutins verts de commencer la partie.").

db_response(combien,
    "Chaque equipe compte 4 lutins.").

db_response(deplacer,
    "Non, vous ne pouvez pas deplacer un lutin sur une case occupee.").

db_response(pont,
    "Apres deplacement, vous pouvez retirer le pont emprunte ou tout autre pont.").

% -----------------------------------------------------------------------------
% produire_reponse(+QuestionIn, -Responses)
%   QuestionIn : atom ou chaîne
%   Responses  : liste d’atomes (ou chaînes) à renvoyer
% -----------------------------------------------------------------------------
produire_reponse(QuestionIn, Responses) :-
    % 1) Prétraitement commun : nettoyage + passage en minuscule
    preprocess(QuestionIn, LowerStr),

    % 2) Cas « conseil » (à implémenter ultérieurement)
    % Si vous voulez gérer dès maintenant les questions de type « conseil »,
    % décommentez et implémentez calculer_conseil/4.
    % re_matchsub("conseill", LowerStr, _, [caseless(true)]),
    % !,
    % calculer_conseil(LowerStr, R1-C1, R2-C2),
    % format(string(Advice),
    %   "Le lutin sur la case (~w,~w) vers la case (~w,~w). \
    %    Puis-je vous conseiller d'enlever le pont (~w,~w)-(~w,~w) ?",
    %   [R1,C1,R2,C2,R1,C1,R2,C2]),
    % Responses = [Advice], !;

    % 3) Réponses statiques via la DB
    (   db_response(Key, Resp),
        atom_string(Key, KeyStr),
        re_matchsub(KeyStr, LowerStr, _, [caseless(true)])
    ->  Responses = [Resp]
    ;   Responses = ["Desole, je n ai pas compris votre question."]
    ).

% -----------------------------------------------------------------------------
% preprocess(+Input, -LowerStr)
%   Nettoie la chaîne d'entrée et la met en minuscules
% -----------------------------------------------------------------------------
preprocess(Input, LowerStr) :-
    % 1) Atom ou string -> string
    ( atom(Input)   -> atom_string(Input, Str)
    ; string(Input) -> Str = Input
    ; throw(error(type_error(string_or_atom, Input), _))
    ),
    % 2) Supprimer la ponctuation
    string_codes(Str, Codes),
    clean_codes(Codes, CleanCodes),
    string_codes(CleanStr, CleanCodes),
    % 3) Minuscules
    string_lower(CleanStr, LowerStr).

% -----------------------------------------------------------------------------
% clean_codes(+InCodes, -OutCodes)
%   Retire tout caractère de type ponctuation
% -----------------------------------------------------------------------------
clean_codes([], []).
clean_codes([C|Cs], Rest) :-
    char_code(Char, C),
    (   char_type(Char, punct)
    ->  clean_codes(Cs, Rest)
    ;   Rest = [C|R], clean_codes(Cs, R)
    ).

% -----------------------------------------------------------------------------
% Placeholder pour le calcul d'un conseil
% À implémenter plus tard : parcourir l'état du jeu pour suggérer
% un déplacement de lutin vert et un pont à retirer.
%
% calculer_conseil(+LowerStr, -FromRow-FromCol, -ToRow-ToCol) :- ...
% -----------------------------------------------------------------------------
