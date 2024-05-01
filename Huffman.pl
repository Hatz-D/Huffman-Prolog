% Função para rodar o programa
main():-
    open('in.txt', read, Arquivo),
    read_string(Arquivo, _, String),
    string_chars(String, Chars),
    loop(Chars, Binario, String),
    remover_duplicatas(Binario, BinarioSemDuplicatas),
    salvar_arquivo(BinarioSemDuplicatas, "out.txt").

salvar_arquivo(String, NomeArquivo) :-
    open(NomeArquivo, write, Arquivo),
    write(Arquivo, String),
    close(Arquivo).

% Função que cria uma lista de tuplas com os caracteres do arquivo e os seus respectivos binários
loop([], [], _).
loop([X | XS], [(X, Binario) | Z], String) :-
    obtem_binario(X, Binario, String),
    loop(XS, Z, String).

% Função para obter o binário de dado caractere
obtem_binario(Caracter, Binario, String):-
    huffman(Arvore, String),
    caractere_binario(Caracter, Arvore, Binario).

% Algoritmo de Huffman
huffman(T2, String):-
    % Converter para uma lista
    atom_chars(String, Lista),
    msort(Lista, ListaOrdenada),
    calcula_frequencia(ListaOrdenada, Pares),

    % Ordena de acordo com a frequência
    sort(Pares, ParesOrdenados),
    cria_arvore(ParesOrdenados, T),
    codificar(T,[],T2).

% Calcula a frequência de cada caractere
calcula_frequencia([],[]).
calcula_frequencia([X|Xs], [Y|Ys]):- quantidade_caractereres(X, Xs, Y, Xs2),
    calcula_frequencia(Xs2, Ys),
    !.

% Relaciona um caractere ao seu respectivo binário na árvore
caractere_binario(X, [[_,X,C]|_], C).
caractere_binario(X, [[_,_,_]|Z], C):-
    caractere_binario(X,Z,C),!.

% Tem como objetivo criar a lista que representa a árvore inteira.
cria_arvore([[C1|X1], [C2|X2]|Xs], Tree):-
    H is C1 + C2,
    Tree1 = [H,[C1|X1], [C2|X2]],
    (   Xs=[] ->
        Tree = Tree1
        ;
        sort([Tree1|Xs], OrderedXs),
        cria_arvore(OrderedXs, Tree)
    ).

% Codifica cada caractere da árvore em binário
codificar([_,E,D], C, L):-
    % Percorre na esquerda (0) até encontrar o nó e o codifica
    ( is_node(E) -> 
        codificar(E, [0|C], L1)
        ;
        codificar_node(E, [0|C], L1)),
    
    % Percorre na direita (1) até encontrar o nó e o codifica
    ( is_node(D) ->
        codificar(D,[1|C], L2)
        ;
        codificar_node(D, [1|C], L2)),
    append(L1, L2, L).

% Verifica se é um nó ou não
is_node([_,_,_]).

% Codifica um nó
codificar_node([N1, N2], Binario, ResultCode):-
    reverse(Binario, ReverseCode),
    atomics_to_string(ReverseCode, NewCode),
    ResultCode = [[N1, N2, NewCode]].

% Obtém a quantidade de caracteres
quantidade_caractereres(X,[],[1,X],[]).
quantidade_caractereres(X,[X|L],[I,X],Z):-
    quantidade_caractereres(X, L, [I2,X],Z), I is I2 + 1.
quantidade_caractereres(X, [Y | L], [1, X], [Y | L]) :-
    X \= Y.
quantidade_caractereres(X, [X | L], [2, X], L).

% Remover as tuplas duplicatas da lista
remover_duplicatas([], []).
remover_duplicatas([(X,Y)|T], Result) :-
    member((X,Y), T), !,
    remover_duplicatas(T, Result).
remover_duplicatas([(X,Y)|T], [(X,Y)|Result]) :-
    remover_duplicatas(T, Result).

% Função para verificar se uma tupla é membro
member(X, [X|_]).
member(X, [_|T]) :- member(X, T).