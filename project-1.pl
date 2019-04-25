% The Knowledge Base for the scheduling assistant.
% Limitations:
%     - genders: man, woman, nonbinary (as an umrella term)
%     - romantic orientations: heteroromantic, homoromantic,
%                              biromantic (as an umbrella term),
%                              aromantic (as an umbrella term),
%                              queer (often used by nonbinary people)
%     - languages: English, French, Spanish, Korean, Mandarin, Samoan, Japanese,
%                  Arabic, Salishan, Thai, Russian
%     - in terms of people speaking multiple languages:
%          + consider only bilingual people; it is assumed that English is one
%            of the languages known
%     - Possible expansions:
%               More requirements: Height, age, archetype
%     - TODO: Make it return false if no one fits a role?

% Actors are represented as actor(X) has a name, gender, romantic orientation,
% and languages they know. Names, genders, romance and languages are defined below.

% List of Actors: (Can add more as you see fit- make sure to fill out every category though)
% Mike: Male, heteroromantic (attracted to Sarah), English
% Jacques: Male, heteroromantic (attracted to Sophia and Zoe), French
% John: Male, biromantic (attracted to Mike, Anna, Zane), English and Arabic
% Juan: Male, homoromantic(attracted to John and Pablo), English and Spanish
% Pablo: Male, homoromantic(attracted to John, Juan and Jacques), English
% Noah: Male, aromantic(attracted to no one), Samoan

% Sarah: Female, heteroromantic (attracted to Mike and Noah), English and Mandarin
% Sophia: Female, heteroromantic (attracted to Pablo), English and Japanese
% Anna: Female, heteroromantic (attracted to Mike, Jacques, John), English and Russian
% Charlette: Female, biromantic (attracted to Zoe, Anna, John, Juan, Mike), French
% Zoe: Female, homoromantic (attracted to Sophia and Sarah), English and Korean

% Zane: nonbinary, biromantic (attracted to Mike and Zoe), English, Thai, and Korean
% Monica: nonbinary, queer (attracted to Zane), Salishan

% predicate to pass on the list of names of the whole cast
castName(c , [mike, jacques, john, juan, pablo, noah, sarah, sophia, anna, charlette, zoe, zane, monica]).
castName(c1 , [jacques, charlette, mike]).
castName(c2 , [jacques, sarah, mike, john]).


% Genders:
% gender(P, X) is true when X is the gender of person P

gender(mike, man).
gender(jacques, man).
gender(john, man).
gender(juan, man).
gender(pablo, man).
gender(noah, man).
gender(sarah, woman).
gender(sophia, woman).
gender(anna, woman).
gender(charlette, woman).
gender(zoe, woman).
gender(zane, nonbinary).
gender(monica, nonbinary).

% Romantic attraction:
% attracted(P1, P2) is true if P1 is romatically attracted to P2.

attracted(mike, sarah).
attracted(jacques, sophia).
attracted(jacques, zoe).
attracted(john, mike).
attracted(john, anna).
attracted(john, zane).
attracted(juan, john).
attracted(juan, pablo).
attracted(pablo, john).
attracted(pablo, juan).
attracted(pablo, jacques).

attracted(sarah, mike).
attracted(sarah, noah).
attracted(sophia, pablo).
attracted(anna, mike).
attracted(anna, jacques).
attracted(anna, john).
attracted(charlette, zoe).
attracted(charlette, anna).
attracted(charlette, john).
attracted(charlette, juan).
attracted(charlette, mike).
attracted(zoe, sophia).
attracted(zoe, sarah).

attracted(zane, mike).
attracted(zane, zoe).
attracted(monica, zane).


% orientation(X, Y) is true if Y is the orientation of X
orientation(X, heteroromantic):- heteroromantic(X).
orientation(X, homoromantic):- homoromantic(X).
orientation(X, queer):- queer(X).
orientation(X, biromantic):- biromantic(X).
orientation(X, aromantic):- aromantic(X).

% heteroromantic(A) is true if:
% A is a man and attracted to women, or
% A is a woman and attrated to men.

heteroromantic(A) :- gender(A, X), gender(B, Y), attracted(A, B), \+ biromantic(A), \+ Y = X.

% homoromantic(B) is true if:
% B is a man and attracted to men, or
% B is a woman and attracted to women.

homoromantic(A) :- gender(A, X), gender(B, X), attracted(A, B), \+ biromantic(A).

% biromantic(C) is true if:
% C is attracted to men and women, or
% C is attracted to men and nonbinary, or
% C is attracted to women and nonbinary, or
% C is attracted to men, women, and nonbinary.

biromantic(A):- gender(X, man), gender(Y, woman), attracted(A, X), attracted(A, Y).
biromantic(A):- gender(X, man), gender(Y, nonbinary), attracted(A, X), attracted(A, Y).
biromantic(A):- gender(X, woman), gender(Y, nonbinary), attracted(A, X), attracted(A, Y).
biromantic(A):- gender(X, man), gender(Y, woman), gender(Z, nonbinary), attracted(A, X), attracted(A, Y), attracted(A, Z).

% aromantic(D) is true if:
% D not attracted to men, women, and nonbinary.

aromantic(D) :- \+ heteroromantic(D), \+ homoromantic(D), \+ biromantic(D), \+ queer(D).

% queer(E) is true if:
% E is nonbinary and attracted to men, or
% E is nonbinary and attracted to women, or
% E is nonbinary and attracted to ninbinary.

queer(E) :- gender(E, nonbinary), gender(X, man), attracted(E, X), \+ biromantic.
queer(E) :- gender(E, nonbinary), gender(X, woman), attracted(E, X), \+ biromantic.
queer(E) :- gender(E, nonbinary), gender(X, nonbinary), attracted(E, X), \+ biromantic.

% Languages:
% speaks(P, Y) is true if person P speaks language Y

speaks(mike, english).
speaks(jacques, english).
speaks(jacques, french).
speaks(john, english).
speaks(john, arabic).
speaks(juan, english).
speaks(juan, spanish).
speaks(pablo, english).
speaks(noah, english).
speaks(noah, samoan).

speaks(sarah, english).
speaks(sarah, mandarin).
speaks(sophia, english).
speaks(sophia, japanese).
speaks(anna, english).
speaks(anna, russian).
speaks(charlette, english).
speaks(charlette, french).
speaks(zoe, english).
speaks(zoe, korean).

speaks(zane, english).
speaks(zane, thai).
speaks(zane, korean).
speaks(monica, english).
speaks(monica, salishan).

% bilingual(P) is true if person P speaks English and another language

bilingual(P) :- speaks(P, english),
                speaks(P, Another_Language),
                \+ Another_Language = english.


% availability(P, L) are the list of days in a week L in which P is available
availability(mike, [monday, tuesday, wednesday, thursday, friday]).
availability(jacques, [monday, tuesday, wednesday, thursday, friday]).
availability(john, [monday, tuesday, wednesday, thursday, friday, saturday, sunday]).
availability(juan, [saturday, sunday]).
availability(pablo, [monday, tuesday, wednesday, thursday, friday, saturday, sunday]).
availability(noah, [monday, tuesday, wednesday, thursday, friday, saturday, sunday]).

availability(sarah, [monday, tuesday, wednesday, thursday, friday, saturday, sunday]).
availability(sophia, [monday, tuesday, wednesday, thursday, friday]).
availability(anna, [monday, wednesday, friday, saturday, sunday]).
availability(charlette, [monday, tuesday, wednesday, thursday, friday, saturday, sunday]).
availability(zoe, [saturday, sunday]).

availability(zane, [monday, tuesday, thursday, saturday, sunday]).
availability(monica, [monday, tuesday, wednesday, thursday, friday, saturday, sunday]).

% archetype(A, L) where L is the list of archetypes that A has experience with
% Possible archtypes : the_innocent, the_hero, the_everyman, the_explorer, the_caregiver, the_rebel, the_mentor,
%                      the_trickster, the_lover, the_ruler, the_expert, the_antihero, the_sociopath

archetype(mike, [the_hero, the_everyman, the_rebel, the_sociopath]).
archetype(jacques, [the_innocent, the_hero, the_explorer, the_rebel, the_mentor, the_trickster, the_lover, the_antihero]).
archetype(john, [the_everyman, the_explorer, the_rebel, the_trickster, the_ruler, the_expert, the_sociopath]).
archetype(juan, [the_caregiver, the_rebel, the_mentor, the_trickster, the_lover, the_ruler, the_sociopath]).
archetype(pablo, [the_innocent, the_hero, the_caregiver, the_mentor, the_expert]).
archetype(noah, [the_lover, the_ruler, the_expert, the_antihero, the_sociopath]).

archetype(sarah, [the_innocent, the_hero, the_everyman, the_explorer, the_rebel, the_lover, the_ruler, the_antihero, the_sociopath]).
archetype(sophia, [the_hero, the_explorer, the_caregiver, the_rebel, the_trickster]).
archetype(anna, [the_rebel, the_mentor, the_trickster, the_lover, the_ruler]).
archetype(charlette, [the_innocent, the_hero, the_everyman, the_explorer, the_caregiver, the_rebel, the_sociopath]).
archetype(zoe, [the_mentor, the_trickster, the_lover, the_ruler, the_expert, the_antihero, the_sociopath]).

archetype(zane, [the_hero, the_everyman, the_rebel, the_mentor, the_lover, the_expert, the_antihero, the_sociopath]).
archetype(monica, [the_innocent, the_hero, the_explorer, the_trickster, the_expert, the_antihero]).


% available_on(P, D) is true if person P is available on day D
available_on(P, D):- availability(P, L), member(D, L).

% plays_archetype(P, A) is true if person P has experience playing archetype A
plays_archetype(P, A):- archetype(P, L), member(A, L).

% role(N, G, O, L, A) is a role with name N, gender G, orientation O, language L, archetype A.

% For Peter Pan:
role(peter, man, aromantic, english, the_trickster).
role(wendy_darling, woman, aromantic, english, the_innocent).
role(captain_hook, man, heteroromantic, english, the_sociopath).
role(croc, animal, aromantic, english, the_explorer).

% For The Little Mermaid:

role(princess_ariel, woman, heteroromantic, english, the_rebel).
role(prince_eric, man, heteroromantic, english, the_lover).
role(ursula, beast, aromantic, english, the_sociopath).
role(sebastian, nonbinary, aromantic, french, the_mentor).
role(king_triton, man, heteroromantic, english, the_caregiver).

% fit_role(R, A, LA, D) finds the best actor A for role R, who does not already have a role in list of actors LA, on date D
% If role is aromantic, then orientation does not matter


fit_role(R, A, LA, D):- castName(c, C), fit_role2(R, C, LA, LR, D), best_suited(LR, -1, _, A).


% fit_role2(R, A, D, LA) creates a list(LA) of actor-rating pair for all actors in A with their ratings(NT) for the role R
% the total rating comes from separate ratings on Gender, Orientation and Archetype
% the actor is not rated if they are not available for the show Day or do not speak the language required for the role

fit_role2(_, [], _, [], _).
fit_role2(R, [A|C], LA, [[A, NT]|LR], D):- role(R, G, O, _, Arch), suited(R, A, D, LA), ratingGen(A, G, N0), ratingOri(A, O, N1), ratingArch(A, Arch, N2), NT is (N0 + N1 + N2), fit_role2(R, C, LA, LR, D).
fit_role2(R, [A|C], LA, LR, D):- role(R, _, _, _, _), \+ suited(R, A, D, LA), fit_role2(R, C, LA, LR, D).

% suited(R, A, D, LA) returns true if the actor is available for the show day D, and speaks the language L required for the role

suited(R, A, D, LA):- role(R, _, _, L, _), speaks(A, L), available_on(A, D), \+ member(A, LA).

% best_suited(T, R, A, BA), for R the current best rating, A current best actor, T list of all actor-rating pairs for the role, returns the actor BA that has the highest rating for the role R

best_suited([], _, RA, RA).

%:- castName(c, C), member(RA, C).

best_suited([[A, R]|T], CR, _, BA):- R > CR, best_suited(T, R, A, BA).
best_suited([[_, R]|T], CR, RA, BA):- R =< CR, best_suited(T, CR, RA, BA).


% ratingGen(A, G, N) takes an actor A and the prefered gender for the role, and returns 1 if they are that gender

ratingGen(A, G, 1):- gender(A, G).
ratingGen(A, G, 0):- \+ gender(A, G).

% ratingOri(A, G, N) takes an actor A and the prefered orientation for the role, and returns 1 if they are that orientation
% returns 1 if the orientation of the role is aromantic (means orientation will not matter)

ratingOri(_, aromantic, 1).
ratingOri(A, O, 1):- orientation(A, O), \+ O = aromantic.
ratingOri(A, O, 0):- \+ orientation(A, O), \+ O = aromantic.

% ratingArch(A, Arch, N) takes an actor A and the archetype of the role, and returns 1 if the actor has experience with
% the archetype and 0 if they do not

ratingArch(A, Arch, 1):- plays_archetype(A, Arch).
ratingArch(A, Arch, 0):- \+ plays_archetype(A, Arch).

% show(Name, D, L) is a show Name on day D, with a list of required roles L.

show(peter_pan, D, [peter, wendy_darling, captain_hook, croc]).
show(the_little_mermaid, D, [princess_ariel, prince_eric, ursula, sebastian, king_triton]).

% fit_show(N, D, LR) finds a list of actors and their role LR in show with name N on date D.

fit_show(N, D, LR):-  show(N, D, L), fit_show2(D, L, LR, []).

% fit_show2(D, T, LR, LA) with show day D, list of role-actor pairs LR (which are already casted for the show), and the list of actors LA that are already casted

fit_show2(_, [], [], _).
fit_show2(D, [H|T], [[H, A]|LR], LA):- fit_role(H, A, LA, D), fit_show2(D, T, LR, [A|LA]).

% try: fit_show(peter_pan, wednesday, LR).
% try: fit_show(the_little_mermaid, saturday, LR).
% try: fit_role2(peter, [mike, jacques, john, juan, pablo, noah, sarah, sophia, anna, charlette, zoe, zane, monica], [], LR,  wednesday).
% try: best_suited([[jacques, 2], [charlette, 1], [mike, 2]], -1, A).

% try: fit_role(wendy_darling, A, []).
% try: fit_role2(wendy_darling, [jacques, charlette, mike], [], LR, wednesday).
% try: best_suited([[jacques, 1], [charlette, 2], [mike, 1]] , -1, A).

% try: fit_role(captain_hook, A, []).
% try: fit_role(captain_hook, A, [jacques]).
% try: fit_role2(captain_hook, [jacques, charlette, mike], LA, wednesday).
% try: best_suited([[jacques, 2], [charlette, 0], [mike, 2]] , -1, _, A).
% try: best_suited([[jacques, 2], [charlette, 0], [mike, 2]] , -1, A, [jacques]).
