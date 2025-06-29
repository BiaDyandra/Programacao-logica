% Fatos dinâmicos: permitem alterar os valores durante a execução
:- dynamic total/1.          % Declara que o predicado 'total/1' pode ser modificado dinamicamente
:- dynamic capacidade/1.     % Declara que o predicado 'capacidade/1' pode ser modificado dinamicamente

% Estado: estado(ME, CE, MD, CD, Lado)
% ME = missionários esquerda
% CE = canibais esquerda
% MD = missionários direita
% CD = canibais direita
% Lado = lado do barco (e/d)

% Estado inicial: todos os missionários e canibais estão na margem esquerda, barco também
inicial(estado(M, C, 0, 0, e)) :- total(M), total(C).  % Define o estado inicial com M missionários e C canibais na esquerda

% Estado final: todos os missionários e canibais foram levados para a margem direita
final(estado(0, 0, M, C, d)) :- total(M), total(C).  % Define o estado final com todos na direita

% Verifica se um estado é válido
valido(estado(ME, CE, MD, CD, _)) :-
    ME >= 0, CE >= 0, MD >= 0, CD >= 0,     % Nenhum valor pode ser negativo
    total(T),                              % Obtém o total de missionários/canibais
    ME =< T, CE =< T, MD =< T, CD =< T,     % Nenhum valor pode ultrapassar o total
    (ME == 0 ; ME >= CE),                  % Missionários à esquerda estão em número seguro
    (MD == 0 ; MD >= CD).                  % Missionários à direita estão em número seguro

% Gera movimentos válidos de M missionários e C canibais que somem até a capacidade do barco
move(M, C) :-
    capacidade(Cap),                 % Pega a capacidade máxima do barco
    between(1, Cap, Total),         % Total de pessoas no barco entre 1 e Cap
    between(0, Total, M),           % Missionários entre 0 e Total
    C is Total - M.                 % Canibais são o restante

% Oper: barco vai da esquerda para a direita
oper(estado(ME, CE, MD, CD, e), estado(NME, NCE, NMD, NCD, d)) :-
    move(M, C),                            % Gera combinação de passageiros
    NME is ME - M, NCE is CE - C,         % Atualiza quantidade à esquerda
    NMD is MD + M, NCD is CD + C,         % Atualiza quantidade à direita
    valido(estado(NME, NCE, NMD, NCD, d)).% Verifica se o novo estado é válido

% Oper: barco volta da direita para a esquerda
oper(estado(ME, CE, MD, CD, d), estado(NME, NCE, NMD, NCD, e)) :-
    move(M, C),                            % Gera combinação de passageiros
    NME is ME + M, NCE is CE + C,         % Atualiza quantidade à esquerda
    NMD is MD - M, NCD is CD - C,         % Atualiza quantidade à direita
    valido(estado(NME, NCE, NMD, NCD, e)).% Verifica se o novo estado é válido

% Procura uma solução com busca em profundidade
solucao(Caminho) :-
    inicial(Ini),                         % Pega o estado inicial
    profundidade(Ini, [Ini], Caminho).   % Chama busca em profundidade com lista de visitados

% Caso base da recursão: chegou ao estado final
profundidade(E, Visitados, Visitados) :- final(E).  % Se o estado atual é final, retorna caminho

% Passo recursivo: gera novo estado, verifica se não foi visitado, continua busca
profundidade(E, Visitados, Caminho) :-
    oper(E, E2),                           % Gera próximo estado
    \+ pertence(E2, Visitados),            % Verifica se não foi visitado ainda
    profundidade(E2, [E2|Visitados], Caminho).  % Continua busca com novo estado

% Predicado pertence: verifica se um elemento está numa lista
pertence(X, [X|_]).                       % Caso base: elemento está na cabeça
pertence(X, [_|Cauda]) :- pertence(X, Cauda).  % Caso recursivo: verifica na cauda

% Exibe o caminho da solução passo a passo
exibir([]).                              % Caso base: lista vazia
exibir([estado(ME, CE, MD, CD, Lado)|T]) :-
    write('Esq: ('), write(ME), write('M,'), write(CE), write('C)  '),  % Exibe lado esquerdo
    write(' Dir: ('), write(MD), write('M,'), write(CD), write('C)  '),  % Exibe lado direito
    write(' Barco: '), write(Lado), nl,                                  % Exibe posição do barco
    exibir(T).                                                           % Continua exibindo o restante

% Predicado principal: resolve o problema com M missionários, C canibais e Cap capacidade
resolver(M, C, Cap) :-
    retractall(total(_)),             % Limpa valores antigos de total
    retractall(capacidade(_)),       % Limpa valores antigos de capacidade
    asserta(total(M)),               % Define novo total de missionários/canibais
    asserta(capacidade(Cap)),        % Define nova capacidade do barco
    solucao(S), !,                   % Busca solução com operador de corte (!)
    write('Caminho da solução:'), nl,  % Exibe cabeçalho
    inverter_lista(S, SR),          % Inverte lista para ordem correta
    exibir(SR).                     % Exibe o caminho

% Caso onde nenhuma solução é encontrada
resolver(_, _, _) :-
    write('Nenhuma solução encontrada.'), nl.  % Mensagem de erro

% Inversão de lista sem usar reverse (usando append do PDF)
inverter_lista([], []).                 % Caso base: lista vazia
inverter_lista([H|T], R) :-            % Divide lista em cabeça e cauda
    inverter_lista(T, RT),            % Inverte o restante
    ap(RT, [H], R).                   % Junta elemento atual no final

% Implementação de append (concatenação de listas), como no PDF
ap([], L, L).                          % Caso base: primeira lista vazia
ap([X|L1], L2, [X|L3]) :- ap(L1, L2, L3).  % Junta elemento X e continua com recursão
