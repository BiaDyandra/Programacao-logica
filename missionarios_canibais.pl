%Missionários e canibais 
%Bianca Dyandra Ribeiro Gomes de Farias 2024.1.08.003
%Projeto 1 - Prolog

:- dynamic total/1.
:- dynamic capacidade/1.

% Estado: estado(ME, CE, MD, CD, Lado)
% ME = missionários esquerda
% CE = canibais esquerda
% MD = missionários direita
% CD = canibais direita
% Lado = lado do barco (e/d)

inicial(estado(M, C, 0, 0, e)) :- total(M), total(C).
final(estado(0, 0, M, C, d)) :- total(M), total(C).

valido(estado(ME, CE, MD, CD, _)) :-
    ME >= 0, CE >= 0, MD >= 0, CD >= 0,
    total(T),
    ME =< T, CE =< T, MD =< T, CD =< T,
    (ME == 0 ; ME >= CE),
    (MD == 0 ; MD >= CD).

move(M, C) :-
    capacidade(Cap),
    between(1, Cap, Total),
    between(0, Total, M),
    C is Total - M.

oper(estado(ME, CE, MD, CD, e), estado(NME, NCE, NMD, NCD, d)) :-
    move(M, C),
    NME is ME - M, NCE is CE - C,
    NMD is MD + M, NCD is CD + C,
    valido(estado(NME, NCE, NMD, NCD, d)).

oper(estado(ME, CE, MD, CD, d), estado(NME, NCE, NMD, NCD, e)) :-
    move(M, C),
    NME is ME + M, NCE is CE + C,
    NMD is MD - M, NCD is CD - C,
    valido(estado(NME, NCE, NMD, NCD, e)).

solucao(Caminho) :-
    inicial(Ini),
    profundidade(Ini, [Ini], Caminho).

profundidade(E, Visitados, Visitados) :- final(E).
profundidade(E, Visitados, Caminho) :-
    oper(E, E2),
    \+ pertence(E2, Visitados),
    profundidade(E2, [E2|Visitados], Caminho).

pertence(X, [X|_]).
pertence(X, [_|Cauda]) :- pertence(X, Cauda).

exibir([]).
exibir([estado(ME, CE, MD, CD, Lado)|T]) :-
    write('Esq: ('), write(ME), write('M,'), write(CE), write('C)  '),
    write(' Dir: ('), write(MD), write('M,'), write(CD), write('C)  '),
    write(' Barco: '), write(Lado), nl,
    exibir(T).

resolver(M, C, Cap) :-
    retractall(total(_)),
    retractall(capacidade(_)),
    asserta(total(M)),
    asserta(capacidade(Cap)),
    solucao(S), !,
    write('Caminho da solução:'), nl,
    inverter_lista(S, SR),
    exibir(SR).

resolver(_, _, _) :-
    write('Nenhuma solução encontrada.'), nl.

inverter_lista([], []).
inverter_lista([H|T], R) :-
    inverter_lista(T, RT),
    ap(RT, [H], R).

ap([], L, L).
ap([X|L1], L2, [X|L3]) :- ap(L1, L2, L3).
