% Marta Costa Braga ist1110034
:- use_module(library(clpfd)). % para poder usar transpose/2
:- set_prolog_flag(answer_write_options,[max_depth(0)]). % ver listas completas
:- ['puzzlesAcampar.pl']. % Ficheiro dado. No Mooshak tera mais puzzles.
% Atencao: nao deves copiar nunca os puzzles para o teu ficheiro de codigo
% Segue-se o codigo



% 4.1 CONSULTAS

/* vizinhanca((L,C), Vizinhanca)
Vizinhanca sao as celulas vizinhas a (L,C)
ordenadas de cima para baixo esquerda para a direita.*/
vizinhanca((L,C), Vizinhanca) :-
    Esq is L - 1,
    Dir is L + 1,
    Cima is C - 1,
    Baixo is C + 1,
    Vizinhanca = [(Esq,C),(L,Cima),(L,Baixo),(Dir,C)].


/* compara(Sinal,(L1,C1),(L2,C2))
Compara duas cordenadas determinando qual deve surgir primeiro.*/
compara(>,(L1,_),(L2,_)) :-
    L1 > L2, 
    !.
compara(<,(L1,_),(L2,_)) :-
    L1 < L2, 
    !.
compara(>,(_,C1),(_,C2)) :-
    C1 > C2,
    !.
compara(<,(_,C1),(_,C2)) :-
    C1 < C2,
    !.


/* vizinhancaAlargada((L,C), VizinhancaAlargada)
VizinhancaAlargada eh a vizinhanca de (L,C) ordenanda 
incluindo tambem as coordenadas diagonais.*/
vizinhancaAlargada((L,C), VizinhancaAlargada) :- !,
    vizinhanca((L,C),Vizinhanca),

    Esq is L - 1, 
    Dir is L + 1,
    Cima is C - 1, 
    Baixo is C + 1,
    Diagonais = [(Esq,Cima),(Esq,Baixo),(Dir,Cima),(Dir,Baixo)], 

    append(Vizinhanca,Diagonais,Viz_Desordenada),
    predsort(compara,Viz_Desordenada,VizinhancaAlargada).


% -------------------------------------------------------AUXILIARES OBJETOS--------------------------------------------------
/* ehObjecto(Objeto)
Objeto eh uma arvore(a), tenda(t) ou relva(r).*/
ehObjecto(Objeto) :- !,
    nonvar(Objeto),
    member(Objeto,[a,t,r]). 


/* ehObjectoIgual(Obj1,Obj2)
Obj1 e Obj2 sao ambos  arvore(a) tenda(t) ou relva(r) 
ou sao ambos variaveis livres.*/
ehObjectoIgual(Obj1,Obj2) :- % arvore,tenda ou relva
    ehObjecto(Obj1),
    ehObjecto(Obj2),
    !,
    Obj1 == Obj2.
ehObjectoIgual(Livre1,Livre2) :- % celula livre
    var(Livre1),
    var(Livre2).


% ----------------------------------------------------------------------------------------------------------------------------
/* todasCelulas/2(Tabuleiro, TodasCelulas)
TodasCelulas sao todas as coordenadas do Tabuleiro ordenadas.*/
todasCelulas(Tabuleiro, TodasCelulas) :- !,
    length(Tabuleiro,Dim),
    findall((L,C),(between(1,Dim,L),between(1,Dim,C)),TodasCelulas).


/* todasCelulas/3(Tabuleiro,TodasCelulas,Objeto)
TodasCelulas eh uma lista com todas as celulas Tabuleiro em que se encontra
o Objeto (a,t ou r) ou uma lista com todas as celulas livres.*/
todasCelulas(Tabuleiro,TodasCelulas,Objeto) :- !,   
    todasCelulas(Tabuleiro,TodasCelTabuleiro),
    findall((L,C),
            (member((L,C),TodasCelTabuleiro),
             nth1(L,Tabuleiro,Linha),
             nth1(C,Linha,ObjetoCel),
             Iguais =..[ehObjectoIgual,Objeto,ObjetoCel],
             Iguais),
            TodasCelulas).


/* contaObjetoLinh(Lin,Objeto,Sum)
Sum eh o numero de vezes que Objeto aparece em Lin.*/
contaObjectoLinha([],_,0) :- !. % caso terminal
contaObjectoLinha([ObjetoCel|R],Objeto,Sum) :- % celula inicial eh igual a Objeto
    ehObjectoIgual(ObjetoCel,Objeto),
    !,
    contaObjectoLinha(R,Objeto,SumNova),
    Sum is SumNova + 1.
contaObjectoLinha([_|R],Objeto,Sum) :- % cabeca nao eh objeto igual a Objeto
    contaObjectoLinha(R,Objeto,Sum).


/* calculaObjectosTabuleiro(Tabuleiro,ContagemLinhas,ContagemColunas,Objeto)
Objeto eh arvore, tenda, relva ou variavel livre.
ContagemLinhas e ContagemColunas sao listas com a quantidade desses objetos
existente por linhas e por colunas, respetivamente.*/
calculaObjectosTabuleiro(Tabuleiro,ContagemLinhas,ContagemColunas,Objeto) :- !,
    % Linhas
    findall(Sum,
            (member(Linha,Tabuleiro),
             contaObjectoLinha(Linha,Objeto,Sum)),
            ContagemLinhas),
    % Colunas
    transpose(Tabuleiro,TransTabuleiro),
    findall(Sum,
            (member(Linha,TransTabuleiro),
             contaObjectoLinha(Linha,Objeto,Sum)),
            ContagemColunas).


/* celulaVazia(Tabuleiro,(L,C))
Tabuleiro tem relva ou uma variavel livre nas coordenadas (L, C)*/
celulaVazia(Tabuleiro,(L,C)) :-
    nth1(L,Tabuleiro,Linha),
    nth1(C,Linha,Objeto),
    nonvar(Objeto),
    Objeto \== r,
    !,
    fail.
celulaVazia(_,_).


% 4.2 INSERCAO TENDAS E RELVA

/* insereObjectoCelula(Tabuleiro,Objeto,(L,C))
Insere o Objeto  nas coordenadas (L,C) do Tabuleiro.*/

insereObjectoCelula(Tabuleiro,TendaOuRelva,(L,C)) :-
    nth1(L, Tabuleiro, Linha),
    nth1(C, Linha, Cel),
    var(Cel),
    !,
    Cel = TendaOuRelva.
insereObjectoCelula(_,_,_).


/* insereObjectoEntrePosicoes(Tabuleiro,Objeto,(L,C1),(L,C2))
Insere o Objeto entre as posicoes C1 e C2 da linha L do Tabuleiro.*/
insereObjectoEntrePosicoes(_,_,(L,C1),(L,C2)) :- % caso terminal
    C1 =:= C2 + 1,
    !.
insereObjectoEntrePosicoes(Tabuleiro,TendaOuRelva,(L,C1),(L,C2)) :- !,
    insereObjectoCelula(Tabuleiro,TendaOuRelva,(L,C1)),
    CNovo is C1 + 1,
    insereObjectoEntrePosicoes(Tabuleiro,TendaOuRelva,(L,CNovo),(L,C2)).


% 4.3 ESTRATEGIAS

/* relva(Puzzle)
O Puzzle tem relva em todas as celulas cujas linhas/colunas 
atingiram o numero maximo de tendas possiveis.*/
relva((Tabuleiro,MaxTenLin,MaxTenCol)) :- !,
    length(Tabuleiro,Dim),
    calculaObjectosTabuleiro(Tabuleiro,TenLin,TenCol,t),

    % Linhas 
    colocaRelva(Tabuleiro,MaxTenLin,TenLin,Dim),

    % Colunas 
    transpose(Tabuleiro,TabuleiroTrans),
    colocaRelva(TabuleiroTrans,MaxTenCol,TenCol,Dim).


/* colocaRelva(Tabuleiro,MaxTen)
Funcao auxiliar que coloca relva nas linhas que atingiram o numero maximo de tendas*/
colocaRelva(Tabuleiro,MaxTen,NumTen,Dim) :- !,
    % RelvaLin eh uma lista das linhas que atingiram o numero maximo de tendas
    findall(N,(nth1(N,MaxTen,Ten),nth1(N,NumTen,Tab),Ten == Tab),RelvaLin), 

    % Coloca relva em todas as linhas que atingiram o numero maximo de tendas
    foreach(member(N,RelvaLin),insereObjectoEntrePosicoes(Tabuleiro,r,(N,1),(N,Dim))).


/* inacessiveis(Tabuleiro)
O Tabuleiro tem relva em todas as posicoes inacessiveis.
(Sao posicoes inacessiveis todas aquelas que nao estiverem na vizinhanca de uma arvore)*/
inacessiveis(Tabuleiro) :- !,
    todasCelulas(Tabuleiro,TodasCelulas),
    todasCelulas(Tabuleiro,TodasArvores,a),

    % Obter lista  de celulas que estao na vizinhanca de arvores (acessiveis)
    findall(CelViz,
            (member(Arvore,TodasArvores),
             vizinhanca(Arvore,VizArv),
             member(CelViz,VizArv)),
            Acessiveis),

    % Obter lista de celulas inacessiveis 
    % (Remover as acessiveis ah lista de todas as celulas do tabuleiro)
    findall(CelInacessivel,
            (member(CelInacessivel,TodasCelulas),
             \+ member(CelInacessivel,Acessiveis)),
            Inacessiveis),

    % Inserir relva nas celulas inacessiveis
    foreach(member(Ina,Inacessiveis),insereObjectoCelula(Tabuleiro,r,Ina)).


/* aproveita(Puzzle)
Coloca tendas em todas as linhas/colunas 
cujo numero de X celulas livres seja igual ao numero de tendas em falta.*/
aproveita((Tabuleiro,MaxTenLin,MaxTenCol)) :- !,
    calculaObjectosTabuleiro(Tabuleiro,TenLin,TenCol,t),
    calculaObjectosTabuleiro(Tabuleiro,VazioLin,VazioCol,_),

    % Linhas
    aproveitaAux(Tabuleiro,MaxTenLin,TenLin,VazioLin),

    % Colunas
    transpose(Tabuleiro,TabuleiroTrans),
    aproveitaAux(TabuleiroTrans,MaxTenCol,TenCol,VazioCol).


/* aproveitaAux(Tabuleiro,MaxTen,NumTen,Vazio)
Funcao auxiliar que coloca tendas em todas as linhas cujo
numero de celulas livres eh igual ao numero de tendas em falta.
MaxTen: numero maximo de tendas por linha
NumTen: numero atual de tendas nessa linha
Vazio: numero de celulas livres nessa linha*/
aproveitaAux(Tabuleiro,MaxTen,NumTen,Vazio) :- !,
    length(Tabuleiro,Dim),

    % Calcula quantas tendas faltam colocar
    findall(FaltaTenLin,
        (nth1(N,MaxTen,Max),
         nth1(N,NumTen,TenAtual),
         FaltaTenLin is Max - TenAtual),
        FaltaTen),

    % Encontra as linhas cujo numero de celulas livres eh igual ao numero de tendas em falta
    findall(N,
            (nth1(N,FaltaTen,Ten),
             nth1(N,Vazio,Vaz),
             Ten == Vaz),
            ColocaTen),

    % Coloca tendas nas celulas livres dessas linhas
    foreach(member(L,ColocaTen),insereObjectoEntrePosicoes(Tabuleiro,t,(L,1),(L,Dim))).


/* limpaVizinhancas(Puzzle)
Coloca relva em todas posicoes ah volta de uma tenda.*/
limpaVizinhancas((Tabuleiro,_,_)) :- !,
    todasCelulas(Tabuleiro,TodasTendas,t),

    % Obter lista de todas as celulas vizinhas a tendas
    findall(CelVizinha,
            (member(Ten,TodasTendas),
             vizinhancaAlargada(Ten,VizancaT),
             member(CelVizinha,VizancaT)),
            VizinhasTen),
    
    % Coloca relva nas celulas vizinhas a tendas
    foreach(member(Cel,VizinhasTen),insereObjectoCelula(Tabuleiro,r,Cel)).


/* unicaHipotese(Puzzle)
Deteta quando resta somente uma celula livre que premite que uma dada
arvore tenha uma tenda associada e coloca uma tenda nessa celula.*/
unicaHipotese((Tabuleiro,_,_)) :- !,
    todasCelulas(Tabuleiro,TodasArvores,a),
    unicaHipotese((Tabuleiro,_,_),TodasArvores).
% Todas as tendas foram vistas (Caso terminal).
unicaHipotese((_,_,_),[]) :- !. 
% Existe uma unica hipotese para colocar uma tenda na vizinhanca da arvore
unicaHipotese((Tabuleiro,_,_),[Arvore|TodasArvores]) :- 
    vizinhanca(Arvore,VizArvore),

    % Calcula o numero de tendas na vizinhanca da arvore
    findall((L,C),
            (member((L,C),VizArvore),
             nth1(L,Tabuleiro,Linha), 
             nth1(C,Linha,Objeto),
             ehObjectoIgual(Objeto,t)),
            Tendas),

    % Verifica se a arvore ainda nao tem uma tenda
    length(Tendas,0),

    % VizLivres: lista de celulas livres na vizinhanca da Arvore
    todasCelulas(Tabuleiro,TodasLivres,_),
    findall(Cel,
            (member(Cel,VizArvore),
             member(Cel,TodasLivres)),
            VizLivres),

    % Verifica que ha apenas uma posicao possivel
    length(VizLivres,1),
    !,

    % Insere tenda na unica celula livre 
    nth1(1,VizLivres,CelulaLivre),
    insereObjectoCelula(Tabuleiro,t,CelulaLivre),

    unicaHipotese((Tabuleiro,_,_),TodasArvores).
% Nao eh possivel defenir uma unica hipotese para a posicao da tenda
unicaHipotese((Tabuleiro,_,_),[_|TodasArvores]) :-
    unicaHipotese((Tabuleiro,_,_),TodasArvores).



% 4.4 TENTATIVA E ERRO

/* Valida(LArv,LTen)
Verdade se a cada arvore na lista LArv pode ser associada
uma unica tenda da lista LTen.*/
valida(LArv,LTen) :-
    % Numero de arvores e de tendas eh igual
    length(LArv,N),
    length(LTen,N),

    validaAux(LArv,LTen).


/*validaAux(LArv,LTen)
Auxiliar do valida. Verifica que cada arvore tem uma tenda correspondente.*/
validaAux([],_):- !.
validaAux([Arv|LArv],LTen):-
    vizinhanca(Arv,VizinhancaArv),

    % Verifica que a tenda Ten esta na vizinhanca de Arv
    member(Ten,VizinhancaArv),
    member(Ten,LTen),

    subtract(LTen,[Ten],LTenResto),
    validaAux(LArv,LTenResto).


/* resolve(Puzzle)
O Puzzle eh um Puzzle resolvido.*/
resolve((Tabuleiro,MaxTenLin,MaxTenCol)) :-
    % Ve se o Tabuleiro tem o numero de tendas certo para cada linha e coluna
    calculaObjectosTabuleiro(Tabuleiro,TenLinhaAtual,TenColunaAtual,t),
    MaxTenLin == TenLinhaAtual,
    MaxTenCol == TenColunaAtual,

    % Calcula o numero de arvores e tendas no tabuleiro
    todasCelulas(Tabuleiro,LArv,a),
    todasCelulas(Tabuleiro,LTen,t),

    % Verifica se eh uma solucao possivel para o tabuleiro
    valida(LArv,LTen),
    !.
resolve((Tabuleiro,MaxTenLin,MaxTenCol)) :- 
    % Calcula o numero de celulas livres inicial
    todasCelulas(Tabuleiro,LivresInico,_),
    length(LivresInico,Inicio),

    % Percorre os predicados que premitem resolver o puzzle
    relva((Tabuleiro,MaxTenLin,MaxTenCol)),
    aproveita((Tabuleiro,MaxTenLin,MaxTenCol)),
    relva((Tabuleiro,MaxTenLin,MaxTenCol)),
    unicaHipotese((Tabuleiro,_,_)),
    limpaVizinhancas((Tabuleiro,_,_)),
    inacessiveis(Tabuleiro),

    % Calcula o numero de celulas livres final
    todasCelulas(Tabuleiro,LivresFim,_),
    length(LivresFim,Fim),

    % Verifica se o tabuleiro foi alterado
    Fim \== Inicio,
    
    resolve((Tabuleiro,MaxTenLin,MaxTenCol)),!.
resolve((Tabuleiro,MaxTenLin,MaxTenCol)) :-
    % Calcula o numero de celulas livres inicial
    todasCelulas(Tabuleiro,LivresInico,_),
    length(LivresInico,Inicio),

    % Percorre os predicados que premitem resolver o puzzle
    relva((Tabuleiro,MaxTenLin,MaxTenCol)),
    aproveita((Tabuleiro,MaxTenLin,MaxTenCol)),
    relva((Tabuleiro,MaxTenLin,MaxTenCol)),
    unicaHipotese((Tabuleiro,_,_)),
    limpaVizinhancas((Tabuleiro,_,_)),
    inacessiveis(Tabuleiro),

    % Calcula o numero de celulas livres final
    todasCelulas(Tabuleiro,LivresFim,_),
    length(LivresFim,Fim),

    % Verifica se o tabuleiro foi alterado
    Fim == Inicio,
    member(X,LivresFim),
    insereObjectoCelula(Tabuleiro,t,X),
    resolve((Tabuleiro,MaxTenLin,MaxTenCol)),!.

