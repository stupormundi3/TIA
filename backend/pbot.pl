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
% produire_reponse(+QuestionIn, -ListOfStrings)
%   QuestionIn : atom ou chaîne
%   ListOfStrings : réponses à renvoyer
% -----------------------------------------------------------------------------
produire_reponse(QuestionIn, Responses) :-
    % 1) récupérer la chaîne
    ( atom(QuestionIn)   -> atom_string(QuestionIn, Str)
    ; string(QuestionIn) -> Str = QuestionIn
    ),
    % 2) nettoyage (ponctuation) et passage en minuscule
    string_codes(Str, Codes),
    clean_codes(Codes, CleanCodes),
    string_codes(CleanStr, CleanCodes),
    string_lower(CleanStr, LowerStr),

    % 3) chercher la clé dans la DB avec une sous-chaîne
    (   db_response(Key, Resp),
        atom_string(Key, KeyStr),
        re_matchsub(KeyStr, LowerStr, _, [caseless(true)])
    ->  Responses = [Resp]
    ;   Responses = ["Desole, je n ai pas compris votre question."]
    ).

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
% Fin du module
% -----------------------------------------------------------------------------
