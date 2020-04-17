% 615:610 Formal Methods for Linguistics
% Base code for the Prolog Problem Set
%
% Example query:
% ?- parse(Tree, [a,linguist,talks]).
%   ==> Tree = "[VP [DP [D a] [NP [N' [N linguist]]]] [V' [V talks]]]"
:- table nb/4.
:- table vb/5.

% Set some Prolog options.
% Changing max_depth from default 10 to 0 means it will write out
% the full tree no matter how deep. The other options remain at
% their default values, but will disappear if not repeated here.

:- set_prolog_flag(answer_write_options,[quoted(true),portray(true),
       max_depth(0),spacing(next_argument)]).

% Difference List Variables
% B - beginning list (including all right context)
% E - end list (the right context without the represented phrase)
% M - a midpoint list (lacking the left constituent)

% *** Sentence Rule ***

sentence(TreeList,B) :- vp(TreeList,_,_,B,[]).

% *** Phrase Structure Rules ***
% vp - verb phrase
% vb - v-bar
% dp - determiner phrase
% np - noun phrase
% nb - n-bar

vp([vp,DP,VB],NUM,gap(dp),B,E) :- dp(DP,NUM,gap(dp),B,M), vb(VB,NUM,_,M,E).
vp([vp,DP,VB],_,nogap,B,E) :- dp(DP,NUM,nogap,B,M), vb(VB,NUM,_,M,E).

vb([vb,IV],NUM,nogap,B,E) :- iv(IV,NUM,B,E).
vb([vb,TV,DP],NUM,GAP,B,E) :- tv(TV,NUM,B,M), dp(DP,_,GAP,M,E).

% 2 
vb([vb,VB,PP],NUM,nogap,B,E) :- vb(VB,NUM,nogap,B,M), pp(PP,M,E).

% dp([dp,DET,NP],NUM,B,E) :- det(DET,B,M), np(NP,NUM,M,E).
dp([dp,DET,NP],NUM,nogap,B,E) :- det(DET,NUM,B,M), np(NP,NUM,M,E).
dp(t,_,gap(dp),E,E).

np([np,NB],NUM,B,E) :- nb(NB,NUM,B,E).
nb([nb,NOUN],NUM,B,E) :- noun(NOUN,NUM,B,E).

% 2
nb([nb,NB,PP],NUM,B,E) :- nb(NB,NUM,B,M), pp(PP,M,E).

% 3
nb([nb,NOUN,CP],NUM,B,E) :- noun(NOUN,NUM,B,M), cp(CP,NUM,_,M,E).

% 2
pp([pp,PREP,DP],B,E) :- prep(PREP,B,M), dp(DP,_,_,M,E).

% 3
rp([rp,REL],B,E) :- rel(REL,B,E).

% 3
cp([cp,RP,CB],NUM,GAP,B,E) :- rp(RP,B,M), cb(CB,NUM,GAP,M,E).
cb([cb,VP],NUM,GAP,B,E) :- vp(VP,NUM,GAP,B,E).


% *** The Lexicon ***
% iv - intransitive verb
% tv - transitive verb
% det - determiner
% noun - noun

iv([iv,talks],sing,[talks|E],E).
iv([iv,talk],plur,[talk|E],E).

tv([tv,sees],sing,[sees|E],E).
tv([tv,see],plur,[see|E],E).

% det([det,a],[a|E],E).
% det([det,the],[the|E],E).
det([det,a],sing,[a|E],E).
det([det,the],_,[the|E],E).

noun([noun,bug],sing,[bug|E],E).
noun([noun,bugs],plur,[bugs|E],E).
noun([noun,linguist],sing,[linguist|E],E).
noun([noun,linguists],plur,[linguists|E],E).
noun([noun,telescope],sing,[telescope|E],E).
noun([noun,telescopes],plur,[telescopes|E],E).

prep([prep,with],[with|E],E).
rel([rel,that],[that|E],E).
%**********************************************************
% Convenience Predicates
%
% These make it easy to run parsing on full sentences,
% and will convert syntactic tree terms into a form
% convenient for tree diagram generation.
%**********************************************************

% Convert phrase markers from prolog notation to more
% familiar notation.
% Examples: nb --> N'
%           noun --> N
%
% This code uses regular expressions, which are provided
% via the PCRE library.

:- use_module(library(pcre)).

convert_markers(Before,After) :-
    re_replace(","/g, " ", Before, A1),
    re_replace("vp"/g, "VP", A1, A2),
    re_replace("vb"/g, "V'", A2, A3),
    re_replace("dp"/g, "DP", A3, A4),
    re_replace("np"/g, "NP", A4, A5),
    re_replace("nb"/g, "N'", A5, A6),
    re_replace("pp"/g, "PP", A6, A7),
    re_replace("rp"/g, "RP", A7, A8),
    re_replace("cp"/g, "CP", A8, A9),
    re_replace("cb"/g, "C'", A9, A10),
    re_replace("noun"/g, "N", A10, A11),
    re_replace("det"/g, "D", A11, A12),
    re_replace("iv"/g, "V", A12, A13),
    re_replace("tv"/g, "V", A13, A14),
    re_replace("prep"/g, "P", A14, A15),
    re_replace("rel"/g, "R", A15, After).

% The top-level call for parsing a sentence.
% WordList is the sentence to be parsed.
% TreeString is a string of the tree suitable for jsSyntaxTree.
% term_string() converts a list to a corresponding string.

parse(TreeString, WordList) :-
    sentence(TreeList, WordList),
    term_string(TreeList, TreeStringPreConversion),
    convert_markers(TreeStringPreConversion, TreeString).
