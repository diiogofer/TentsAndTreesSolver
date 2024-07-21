%  Diogo Fernandes (diiogofer - https://github.com/diiogofer)
:- use_module(library(clpfd)). 
:- set_prolog_flag(answer_write_options,[max_depth(0)]). 
:- ['puzzlesAcampar.pl']. 


/*
ASSINATURA PUZZLE "TENDAS E ARVORES" .Consultas
Predicados Obrigatorios e Auxiliares

---> vizinhanca(Coordenada, ListaDeCoordenadasDaVizinhanca)
---> vizinhancaAlargada(Coordenada,ListaDeCoordenadasDaVizinhancaAlargada)
    -> vizinhaAux(Linha, Coluna, Cima, Esquerda, Direita, Baixo)
    -> encontraVizinhancaListaCoord(ListaDeCoordenadas, Vizinhanca) -> vizinhanca de uma lista de coordenadas
    -> encontraVizinhancaAlargadaListaCoord(ListaCoordenadas, Vizinhanca) -> vizinhanca alargada de uma lista de coordenadas

---> todasCelulas/2 -> (Tabuleiro, ListaComTodasAsCelulas)
---> todasCelulas/3 -> (Tabuleiro, ListaComTodasAsCelulasComObjecto, Objecto)

---> calculaObjectosTabuleiro(Tabuleiro, ContagemLinhas, ContagemColunas, Objecto)
    -> calculaObjectosLista(Lista, Objecto, Contagem)
    -> calculaObjectoTodasLinhas(Tabuleiro, ListaContagemLinhas, Objecto)

---> celulaVazia(Tabuleiro, Coordenada) 
*/

%CONSULTAS

%PREDICADOS DAS VIZINHANCAS 
% vizinhanca, vizinhancaAlargada, vizinhaAux 
vizinhanca((Linha, Coluna),          [(Cima, Coluna), 
                    (Linha,Esquerda),                (Linha, Direita), 
                                     (Baixo,Coluna)]) :-
    vizinhaAux(Linha , Coluna, Cima, Esquerda, Direita, Baixo).


vizinhancaAlargada((Linha, Coluna), [(Cima, Esquerda), (Cima, Coluna), (Cima, Direita), 
                                    (Linha, Esquerda),                 (Linha, Direita),  
                                    (Baixo, Esquerda), (Baixo, Coluna), (Baixo, Direita)]) :-
    vizinhaAux(Linha , Coluna, Cima, Esquerda, Direita, Baixo).


vizinhaAux(Linha , Coluna, Cima, Esquerda, Direita, Baixo) :-
                            Cima is Linha - 1, 
    Esquerda is Coluna - 1,                   Direita is Coluna + 1,
                            Baixo is Linha + 1.


% encontraVizinhancaListaCoord(ListaDeCoordenadas, Vizinhanca)
% (Usado em inacessiveis/1) 
encontraVizinhancaListaCoord([],[]) :- !.
encontraVizinhancaListaCoord([Coordenada | RestoCoordenadas], [Vizinhas | RestoDasVizinhas]) :-
    vizinhanca(Coordenada, Vizinhas),
    encontraVizinhancaListaCoord(RestoCoordenadas, RestoDasVizinhas).

%encontraVizinhancaAlargadaListaCoord(ListaCoordenadas, Vizinhanca)
% (Usado em limpaVizinhancas/1)
encontraVizinhancaAlargadaListaCoord([],[]) :- !.
encontraVizinhancaAlargadaListaCoord([Coordenada | RestoCoordenadas], [VizinhancaAlargada | Resto]) :-
    vizinhancaAlargada(Coordenada, VizinhancaAlargada),
    encontraVizinhancaAlargadaListaCoord(RestoCoordenadas, Resto).
% Nota -> Cada vizinhanca esta numa sublista individual


%PREDICADOS TODASCELULAS (/2 e /3)
todasCelulas(Tabuleiro, TodasCelulas) :-
    length(Tabuleiro, Comprimento),
    findall((Linha,Coluna), (between(1, Comprimento, Linha), between(1,Comprimento, Coluna)), TodasCelulas).

todasCelulas(Tabuleiro, TodasCelulas, Objeto) :-
    nonvar(Objeto),
    findall((Linha, Coluna), (nth1(Linha, Tabuleiro, LinhaE), nth1(Coluna, LinhaE, Elemento), 
                            Elemento == Objeto), TodasCelulas), !;
    findall((Linha, Coluna), (nth1(Linha, Tabuleiro, LinhaE), nth1(Coluna, LinhaE, Elemento), 
                            Elemento \== a, Elemento \== r, Elemento \== t), TodasCelulas).

%Predicados Auxiliares para calculaObjectosTabuleiro
%calculaObjectosLista(Lista, Objecto, Contagem)
calculaObjectosLista([], _, 0).
%Se obecto for uma constante que se procura
calculaObjectosLista([Elemento | Resto], Objecto, N) :-
    nonvar(Objecto),
    Elemento == Objecto,
    calculaObjectosLista(Resto, Objecto, Nanterior),
    N is Nanterior + 1.

calculaObjectosLista([Elemento | Resto], Objecto, N) :-
    nonvar(Objecto),
    Elemento \== Objecto,
    calculaObjectosLista(Resto, Objecto, N).
%Caso em que Objeto e uma variavel (e no tabuleiro aparece _)
calculaObjectosLista([Elemento | Resto], Objecto, N) :-
    var(Objecto),
    (Elemento \== a, Elemento \== r, Elemento \== t),
    calculaObjectosLista(Resto, Objecto, Nanterior),
    N is Nanterior + 1.

calculaObjectosLista([Elemento | Resto], Objecto, N) :-
    var(Objecto),
    (Elemento == a; Elemento == r; Elemento == t),
    calculaObjectosLista(Resto, Objecto, N).

%calculaObjectosTodasLinhas(Tabuleiro, ListaContagemLinhas, Objecto)
calculaObjectosTodasLinhas([],[],_).

calculaObjectosTodasLinhas([Linha | Linhas], [Nlinhas | RestoNLinhas] , Objecto) :-
    calculaObjectosLista(Linha, Objecto, Nlinhas),
    calculaObjectosTodasLinhas(Linhas, RestoNLinhas, Objecto). 

%Predicado Principal - calculaObjectosTabuleiro(Tabuleiro, ContagemLinhas, ContagemColunas, Objecto)
calculaObjectosTabuleiro(Tabuleiro, CLinhas, CColunas, Objecto) :-
    calculaObjectosTodasLinhas(Tabuleiro, CLinhas, Objecto),
    transpose(Tabuleiro, TabuleiroTransposto),
    calculaObjectosTodasLinhas(TabuleiroTransposto, CColunas, Objecto) , !.

%PREDICADO CELULAVAZIA - celulavazia(Tabuleiro, (L,C))
celulaVazia(Tabuleiro, (L,C)) :-
    (length(Tabuleiro, Tamanho), (L =< 0 ; L > Tamanho ; C =< 0 ; C > Tamanho)), !;
    (nth1(L, Tabuleiro, Linha), nth1(C, Linha, Elemento), Elemento \== a, Elemento \== t), !.


/*
ASSINATURA PUZZLE "TENDAS E ARVORES" .Insere
Predicados Obrigatorios e Auxiliares

---> insereObjectoCelula(Tabuleiro, TendaOuRelva, Coordenada)
---> insereObjectoEntrePosicoes(Tabuleiro, TendaOuRelva, Coordenada1, Coordenada2)
    -> insereObjectoEmMultiplasCelulas(Tabuleiro, Objecto, ListaDeCoordenadas)
*/

%.INSERE

% insereObjectoCelula(Tabuleiro, TendaOuRelva, Coordenada)
insereObjectoCelula(Tabuleiro, TendaOuRelva, (L,C)) :-
    (nth1(L, Tabuleiro, Linha), nth1(C, Linha, Elemento),
    %caso se possa colocar TendaOuRelva
    ((Elemento \== a, Elemento \== t, Elemento = TendaOuRelva), !;
    %Caso o elemento ja seja uma arvore ou uma tenda
    ((Elemento == a), !; (Elemento == t), !; (Elemento == r)), !));
    %Caso Coordenadas Fora da Matriz
    (length(Tabuleiro, Tamanho), (L =< 0 ,! ; L > Tamanho, !; C =< 0 ,! ; C > Tamanho, !)).

% insereObjectoEntrePosicoes(Tabuleiro, TendaOuRelva, Coordenada1, Coordenada2)
insereObjectoEntrePosicoes(_, _, (Linha, ColunaSeguinte), (Linha,Coluna1)) :- ColunaSeguinte is Coluna1 + 1.
insereObjectoEntrePosicoes(Tabuleiro, TendaOuRelva, (Linha, Coluna1), (Linha ,Coluna2)) :-
    %Coordenadas fora do Tabuleiro 
    (length(Tabuleiro, Tamanho), (Linha =< 0 ; Linha > Tamanho ; Coluna1 =< 0 ; Coluna1 > Tamanho ; Coluna2 =< 0 ; Coluna2 > Tamanho) ,! );
    %Coordenadas no Tabuleiro
    (Coluna1 =< Coluna2, insereObjectoCelula(Tabuleiro, TendaOuRelva, (Linha ,Coluna1)),
    ColunaSeguinte is Coluna1 + 1, insereObjectoEntrePosicoes(Tabuleiro, TendaOuRelva, (Linha, ColunaSeguinte), (Linha, Coluna2))) ,!.

% insereObjectoEmMultiplasCelulas(Tabuleiro, Objecto, ListaDeCoordenadas)
insereObjectoEmMultiplasCelulas(_, _,[]) :- !.
insereObjectoEmMultiplasCelulas(Tabuleiro, Objecto, [Coordenadas | RestoCoordenadas]) :-
    insereObjectoCelula(Tabuleiro, Objecto, Coordenadas),
    insereObjectoEmMultiplasCelulas(Tabuleiro, Objecto, RestoCoordenadas).

/*
ASSINATURA PUZZLE "TENDAS E ARVORES" .Estrategias
Predicados Obrigatorios e Auxiliares
    -> auxIndiceComum(Tabuleiro, TabuleiroTransposto, TamanhoTabuleiro, Objecto, IndicesComunsLinhas, IndicesComunsColunas)
        Coloca Objecto nas linhas e colunas das listas IndicesComunsLinhas e IndicesComunsColunas.
        Auxiliar Principal da parte de estrategias: usado em relva/1 e aproveita/1.
    
    -> posicoeslivres(Tabuleiro, TamanhoTabuleiro, ListaVizinhancas, ListaLivre)
        Ajuda na obtencao da posicao unica que permite que uma dada arvore tenha uma tenda nessa posicao.
        Usado no unicaHipotese/1.

---> relva(Puzzle) -> relva onde numero de tendas igual ao numero possivel.
    -> utlizia o auxiliar auxIndiceComum/6.

---> inacessiveis(Tabuleiro) -> relva nas posicoes inacessiveis.
    -> utiliza os auxiliares encontraVizinhancaListaCoord/2 e insereObjectoEmMultiplasCelulas/3.

---> aproveita(Puzzle) -> tendas onde existem X posicoes livres e exitem X tendas possiveis.
    -> utiliza o auxiliar auxIndiceComum/6 e o subtrai/3.
    ->subtrai/3 -> Dadas duas listas, subtrai a lista dois da lista um elemento a elemento. 
        Ex: L1 = [1,2,3], L2 = [1,1,1], LResultante = [0,1,2].

---> limpavizinhancas(Puzzle) -> relva em todas as posicoes a volta de uma tenda.
    -> utiliza o encontraVizinhancaAlargadaListaCoord/2.

---> unicaHipotese(Puzzle) ->  coloca uma tenda na vizinhanca de todas as arvores que tenham apenas um posicao
    livre na sua vizinhanca e que ainda possam ter uma tenda conectada.
    -> utiliza o posicoeslivres/4.
*/

%.ESTRATEGIAS

% auxIndiceComum(Tabuleiro, TabuleiroTransposto, TamanhoTabuleiro, Objecto, IndicesComunsLinhas, IndicesComunsColunas)
auxIndiceComum(_, _, _, _,[], []).
%Colocacao do Objecto nas respetivas linhas (usando o Tabuleiro) e colunas (usando o TabuleiroTransposto)
auxIndiceComum(Tabuleiro, TabuleiroTransposto, TamanhoTabuleiro, Objecto, [IndiceComumLinha | IndicesComunsLinhas], [IndiceComumColuna | IndicesComunsColunas]) :-
    insereObjectoEntrePosicoes(Tabuleiro, Objecto, (IndiceComumLinha, 1), (IndiceComumLinha, TamanhoTabuleiro)),
    insereObjectoEntrePosicoes(TabuleiroTransposto, Objecto, (IndiceComumColuna, 1), (IndiceComumColuna, TamanhoTabuleiro)),
    auxIndiceComum(Tabuleiro, TabuleiroTransposto, TamanhoTabuleiro, Objecto, IndicesComunsLinhas, IndicesComunsColunas).
%Caso a lista das linhas fique vazia primeiro:
auxIndiceComum(Tabuleiro, TabuleiroTransposto, TamanhoTabuleiro, Objecto, [IndiceComumLinha | IndicesComunsLinhas], IndicesComunsColunas) :-
    insereObjectoEntrePosicoes(Tabuleiro, Objecto, (IndiceComumLinha, 1), (IndiceComumLinha, TamanhoTabuleiro)),
    auxIndiceComum(Tabuleiro, TabuleiroTransposto, TamanhoTabuleiro, Objecto,  IndicesComunsLinhas, IndicesComunsColunas).
%Caso a lista das colunas fique vazia primeiro:
auxIndiceComum(Tabuleiro, TabuleiroTransposto, TamanhoTabuleiro, Objecto, IndicesComunsLinhas, [IndiceComumColuna | IndicesComunsColunas]) :-
    insereObjectoEntrePosicoes(TabuleiroTransposto, Objecto, (IndiceComumColuna, 1), (IndiceComumColuna, TamanhoTabuleiro)),
    auxIndiceComum(Tabuleiro, TabuleiroTransposto, TamanhoTabuleiro, Objecto, IndicesComunsLinhas, IndicesComunsColunas).


% posicoeslivres(Tabuleiro, TamanhoTabuleiro, ListaVizinhancas, ListaLivre)
posicoesLivres(_ , _, [], []) :- !.

posicoesLivres(Tabuleiro, TamanhoTabuleiro, [ListaVizinhanca | ListaVizinhancas], [ListaLivre | Resto]) :-
    %Obter a parte valida da vizinhanca (de arvores)
    findall((L,C),  (member((L,C), ListaVizinhanca), (L > 0, L =< TamanhoTabuleiro, C > 0,  C =< TamanhoTabuleiro)), VizinhancaValida),
    %Obter uma lista livre com um elemento (um par coordenado)
    findall((L,C), (member((L,C), VizinhancaValida), nth1(L, Tabuleiro, Linha), nth1(C, Linha, Elemento), 
                    Elemento \== a, Elemento \== r, Elemento \== t), ListaLivre),
    length(ListaLivre, 1), ListaLivre = [(L,C)],
    %Verifica se a vizinhanca tem tenda e se tiver nao considera a posicao como livre. (L1,C1) tambem sao coordenadas.
    findall((L1,C1) , (member((L1,C1), VizinhancaValida), nth1(L1, Tabuleiro, Linha2), nth1(C1, Linha2, Elemento2), Elemento2 == t), LigadaArvoreTenda), 
    length(LigadaArvoreTenda, Comprimento), Comprimento =:= 0,
    posicoesLivres(Tabuleiro, TamanhoTabuleiro, ListaVizinhancas, Resto).

posicoesLivres(Tabuleiro, TamanhoTabuleiro, [_ | ListaVizinhancas], ListaLivre) :-
    posicoesLivres(Tabuleiro, TamanhoTabuleiro, ListaVizinhancas, ListaLivre).


% relva((Puzzle)), Puzzle = ((Tabuleiro, NLinhas, NColunas)
relva((Tabuleiro, NLinhas, NColunas)) :-
    %Contagem de tendas e obtencao de indices que tem o mesmo numero de tendas no mesmo indice (entre as listas de contagem e de possiveis tendas)
    calculaObjectosTabuleiro(Tabuleiro, CLinhas, CColunas, t),
    findall(Indice, (nth1(Indice, NLinhas, Elemento1), nth1(Indice, CLinhas, Elemento1)), IndicesComunsLinha),
    findall(Indice, (nth1(Indice, NColunas, Elemento2), nth1(Indice, CColunas, Elemento2)), IndicesComunsColuna),
    %Preparacao para o auxIndiceComum/6 e aplicacao do mesmo
    transpose(Tabuleiro, TabuleiroTransposto),
    length(Tabuleiro, TamanhoTabuleiro),
    auxIndiceComum(Tabuleiro, TabuleiroTransposto, TamanhoTabuleiro, r, IndicesComunsLinha, IndicesComunsColuna), !.

% inacessiveis(Tabuleiro)
inacessiveis(Tabuleiro) :- 
    %Encontra as coordenadas de todas as arvores e as duas vizinhancas
    todasCelulas(Tabuleiro, ListaCoordArvores, a),
    encontraVizinhancaListaCoord(ListaCoordArvores, ListasCoordVizArvores),
    flatten(ListasCoordVizArvores, ListaCoordVizArvores), % --> Cada vizinhanca esta numa sublista 
    %Remocao das coordenadas invalidas
    length(Tabuleiro, Tamanho),
    findall((L, C),  (member((L,C), ListaCoordVizArvores), (L > 0, L =< Tamanho, C > 0,  C =< Tamanho)), ListaCoordVizArvoresValida),
    %Ideia da Lista Diferenca: TodasCelulas - Lista das vizinhancas das arvores = Lista das inacessiveis
    todasCelulas(Tabuleiro, TodasCelulas),
    subtract(TodasCelulas, ListaCoordVizArvoresValida, ListaDiferenca),
    %Insercao de relva nas inacessiveis
    insereObjectoEmMultiplasCelulas(Tabuleiro, r, ListaDiferenca), !.

%subtrai(Lista1, Lista2, ListaResultante)
subtrai([],[],[]).
subtrai([ElementoA | RestoA], [ElementoB | RestoB], [Resultado | RestoRes]):-
    Resultado is ElementoA - ElementoB,
    subtrai(RestoA, RestoB, RestoRes).

% aproveita((Tabuleiro, NLinhas, NColunas))
aproveita((Tabuleiro, NLinhas, NColunas)) :-
    %NPossivelDeTendas - ContagemDeTendas tem de ser igual a NEspacosVazios para colocar tenda
    calculaObjectosTabuleiro(Tabuleiro, CLinhasTenda, CColunasTenda, t),
    calculaObjectosTabuleiro(Tabuleiro, CLinhas, CColunas, _),
    subtrai(NLinhas, CLinhasTenda, ResLinhas), subtrai(NColunas, CColunasTenda, ResColunas),
    %Calcular Indices para os quais as listas tem os mesmos elementos na mesma posicao
    findall(Indice, (nth1(Indice, ResLinhas, Elemento1), nth1(Indice, CLinhas, Elemento1)), IndicesComunsLinha),
    findall(Indice, (nth1(Indice, ResColunas, Elemento2), nth1(Indice, CColunas, Elemento2)), IndicesComunsColuna),
    %Preparacao para o auxIndiceComum/6 e utilizacao do mesmo
    transpose(Tabuleiro, TabuleiroTransposto),
    length(Tabuleiro, TamanhoTabuleiro),
    auxIndiceComum(Tabuleiro, TabuleiroTransposto, TamanhoTabuleiro, t, IndicesComunsLinha, IndicesComunsColuna), !.

limpaVizinhancas((Tabuleiro, _, _)) :-
    %Obtem as coordenadas de todas as tendas e respetiva vizinhanca alargada
    todasCelulas(Tabuleiro, ListaCoordTendas, t),
    encontraVizinhancaAlargadaListaCoord(ListaCoordTendas, ListasCoordVizinhancaAlargadaTendas),
    flatten(ListasCoordVizinhancaAlargadaTendas, ListaCoordVizinhancaAlargadaTendas),
    %Vizinhanca alargada valida
    length(Tabuleiro, Tamanho),
    findall((L, C),  (member((L,C), ListaCoordVizinhancaAlargadaTendas), (L > 0, L =< Tamanho, C > 0,  C =< Tamanho)), ListaCoordVizTendasValida),
    %Insercao de relva nas coordenadas da vizinhanca alargada das tendas
    insereObjectoEmMultiplasCelulas(Tabuleiro, r, ListaCoordVizTendasValida).

unicaHipotese((Tabuleiro, _, _)) :- 
    %Obtem todas as arvores e todas as viznhancas das mesmas
    todasCelulas(Tabuleiro, CoordArvores, a),
    encontraVizinhancaListaCoord(CoordArvores, VariasListasVizinhancas),
    %Obtem as posicoes livres
    length(Tabuleiro, ComprimentoMatriz),
    posicoesLivres(Tabuleiro, ComprimentoMatriz, VariasListasVizinhancas, ListaLivre) ,! ,
    flatten(ListaLivre, ListaLivreFlat), %(ListaLivre = [[(L,C)],...])
    %Insere tenda nas posicoes livres que ainda podem ter tenda
    insereObjectoEmMultiplasCelulas(Tabuleiro, t, ListaLivreFlat).


/*
ASSINATURA PUZZLE "TENDAS E ARVORES" .Tentativa e Erro
Predicados Obrigatorios e Auxiliares

    -> tendaNaVizinhancaAlargada(LTendas) (e TendaNaVizinhancaAlargadaAux/3) -> false caso exista uma tenda na vizinhanca alargada de outra
    -> nonmember(Arg, Lista) -> Verifica se Arg e membro da Lista (Predicado de biblioteca de Prolog)
    -> minArvorePorTenda(LArvores, LTendas) -> pelo menos uma arvore para cada tenda
    -> peloMenosUmElementoComum(Lista1, Lista2) -> verifica se duas listas tem pelo menos um elemento em comum
---> valida(ListaArvores,ListaTendas) -> existe uma e uma unica tenda para cada arvore nas suas vizinhacas
        
    -> heuristicas(Puzzle) -> aplica todos os predicados obrigatorios das estrategias num Puzzle.
    ->loopHeuristicas(Puzzle) -> aplica heuristicas/1 ate que o Puzzle permaneca inalterado.
    -> confirmaSolucao(Puzzle) -> confirma a solucao de um Puzzle.
    -> insereTendaRandom(Puzzle, ListaDePosicoesParaPossiveisTendas) -> Insere uma tenda aleatoria e tenda resolver puzzle
---> resolve(Puzzle) -> aplica os predicados loopHeuristicas/1, insereTendaRandom/2 e confirmaSolucao/2 de modo a resolver o puzzle.
*/

%.TENTATIVA E ERRO

%tendaNaVizinhancaAlargada(LTendas) (e TendaNaVizinhancaAlargadaAux/3)
tendaNaVizinhancaAlargada([]) :- !.
tendaNaVizinhancaAlargada([Tenda | Tendas]) :-
    vizinhancaAlargada(Tenda, VizinhasAlargadas),
    tendaNaVizinhancaAlargadaAux(Tenda, Tendas, VizinhasAlargadas),
    tendaNaVizinhancaAlargada(Tendas).
tendaNaVizinhancaAlargadaAux(_, [], _).
tendaNaVizinhancaAlargadaAux(Tenda, [OutraTenda | RestoTendas], VizinhasAlargadas) :-
    nonmember(OutraTenda, VizinhasAlargadas),
    tendaNaVizinhancaAlargadaAux(Tenda, RestoTendas, VizinhasAlargadas).

%Nota: Predicado nonmember/2 retirado de uma biblioteca de Prolog
nonmember(Arg, [Arg|_]) :-
        !,
        fail.
nonmember(Arg,[_|Tail]) :-
        !,
        nonmember(Arg,Tail).
nonmember(_,[]).

% minArvorePorTenda(LArvores, LTendas). 
minArvorePorTenda(_, []) :- !.
minArvorePorTenda(LArvores, [Tenda | Tendas]) :-
    vizinhanca(Tenda, VizinhancaDaTenda),
    peloMenosUmElementoComum(LArvores, VizinhancaDaTenda),
    minArvorePorTenda(LArvores, Tendas).

% peloMenosUmElementoComum(Lista1,Lista2)
peloMenosUmElementoComum(Lista1, Lista2) :-
    member(X, Lista1),
    member(X, Lista2).

%valida(ListaDeCoordenadasDasArvores, ListaDeCoordenadasDasTendas)
valida(ListaArvores , ListaTendas) :-
    %Se nao houver o mesmo numero de Tendas e Arvores sabe-se logo que e falso
    length(ListaArvores, Comprimento1), length(ListaTendas, Comprimento2), Comprimento1 =:= Comprimento2,
    %Verifica Se Existem Arvores Nas Vizinhancas alargadas umas das outras (nao pode acontecer)
    tendaNaVizinhancaAlargada(ListaTendas),
    %Verifica se existe pelo menos uma arvore na vizinhanca de cada tenda
    minArvorePorTenda(ListaArvores, ListaTendas), !.

% heuristicas(Puzzle)
heuristicas((Tabuleiro, NLinhas, NColunas)):-
    inacessiveis(Tabuleiro),
    relva((Tabuleiro, NLinhas, NColunas)), 
    aproveita((Tabuleiro, NLinhas, NColunas)), 
    relva((Tabuleiro, NLinhas, NColunas)), 
    unicaHipotese((Tabuleiro, NLinhas, NColunas)),
    limpaVizinhancas((Tabuleiro, NLinhas, NColunas)).

% confirmaSolucao(Puzzle)
confirmaSolucao((Tabuleiro, NLinhas, NColunas)) :-
    calculaObjectosTabuleiro(Tabuleiro, CLinhas, CColunas, t),
    CLinhas == NLinhas, CColunas == NColunas,
    %Valida (ja verifica se existem tendas conectadas)
    todasCelulas(Tabuleiro, LArvores, a),
    todasCelulas(Tabuleiro, LTendas, t),
    valida(LArvores, LTendas).

% loopHeuristicas(Puzzle)
loopHeuristicas((Tabuleiro, NLinhas, NColunas)) :-
    %Numero de Celulas Vazias antes das heuristicas
    todasCelulas(Tabuleiro, TodasCelulas, X),
    length(TodasCelulas, NumeroDeVaziosTabuleiro),
    %Heuristicas
    heuristicas((Tabuleiro, NLinhas, NColunas)),
    %Numero de Celulas Vazias depois das heuristicas
    todasCelulas(Tabuleiro, TodasCelulas2, X),
    length(TodasCelulas2, NumeroDeVaziosTabuleiro2),
    %Se o Numero Celulas Vazias permanecer igual, o loop acaba
    NumeroDeVaziosTabuleiro =\= NumeroDeVaziosTabuleiro2,
    loopHeuristicas((Tabuleiro, NLinhas, NColunas)).

loopHeuristicas(_).


%insereTendaRandom(Puzzle, ListaDeCoordenadasParaTendas)
insereTendaRendom(_,[]).
%Insercao da Tenda e tentativa de resolucao do Puzzle
insereTendaRandom(P, [Coordenada | _]):-
    P = (Tabuleiro, _, _),
    insereObjectoCelula(Tabuleiro, t, Coordenada),
    loopHeuristicas(P),
    confirmaSolucao(P).
%Caso nao tenha sido possivel resolver o puzzle -> insercao de nova tenda e repeticao do processo
insereTendaRandom(P, [_ | Coordenadas]) :-
    insereTendaRandom(P, Coordenadas).


%resolve(Puzzle)

%Confirmacao inicial da solucao
resolve(Puzzle) :-
    confirmaSolucao(Puzzle), !.
%Confirmacao falhou -> tentativa de resolucao com heuristicas e nova confirmacao
resolve(Puzzle) :-
    loopHeuristicas(Puzzle),
    confirmaSolucao(Puzzle).
%Confirmacao falhou -> tentativa de resolucao com "Forca Bruta"
resolve((Tabuleiro, NLinhas, NColunas)) :-
    todasCelulas(Tabuleiro, ListaArvores, a),
    encontraVizinhancaListaCoord(ListaArvores, Vizinhanca),
    flatten(Vizinhanca, VizinhancaFlat),
    insereTendaRandom((Tabuleiro, NLinhas, NColunas), VizinhancaFlat), !.
%Nota -> Insere Tenda Random ja confirma a Solucao
