% Nuno Goncalves (ist1103392)

:- [codigo_comum].

% -----------------------------------------------------------------------------
% extrai_ilhas_linha(N_L, Linha, Ilhas)
% N_L corresponde ao numero de uma linha, Linha eh uma lista correspondente a
% uma linha de um puzzle e Ilhas eh a lista ordenada cujos elementos sao as
% ilhas da linha Linha.
% -----------------------------------------------------------------------------
extrai_ilhas_linha(N_L, Linha, Ilhas) :-
    findall(ilha(N_Pontes, (N_L, Coluna)), 
    (nth1(Coluna, Linha, N_Pontes), N_Pontes \== 0), Ilhas).

% -----------------------------------------------------------------------------
% ilhas(Puz, Ilhas)
% Puz eh um puzzle e Ilhas eh a lista ordenada cujos elementos sao as ilhas de
% Puz.
% -----------------------------------------------------------------------------
ilhas(Puz, Ilhas) :-
    findall(Ilhas_L, (nth1(N_L, Puz, Linha), 
    extrai_ilhas_linha(N_L, Linha, Ilhas_L)), Lista_Ilhas),
    append(Lista_Ilhas, Ilhas).

% -----------------------------------------------------------------------------
% trata_lista_viz(Lista_Viz, Direcao, Vizinha)
% Lista_Viz eh uma lista correspondente as ilhas vizinhas numa certa
% Direcao(1-Cima; 2-Esquerda; 3-Direita; 4-Baixo) e Vizinha eh a ilha vizinha
% mais proxima nessa direcao.
% -----------------------------------------------------------------------------
trata_lista_viz([], _, []) :- !.
trata_lista_viz(Lista_Viz, 1, [Vizinha]) :- last(Lista_Viz, Vizinha), !.
trata_lista_viz(Lista_Viz, 2, [Vizinha]) :- last(Lista_Viz, Vizinha), !.
trata_lista_viz([Vizinha | _], 3, [Vizinha]) :- !.
trata_lista_viz([Vizinha | _], 4, [Vizinha]) :- !.

% -----------------------------------------------------------------------------
% posicao_ilha(Ilha, Pos).
% Pos eh a posicao da Ilha.
% -----------------------------------------------------------------------------
posicao_ilha(ilha(_, (X, Y)), (X, Y)).

% -----------------------------------------------------------------------------
% posicao_lista(Pos, Lista_Coordenadas).
% Lista_Coordenadas eh a lista correspondente as coordenadas de Pos.
% -----------------------------------------------------------------------------
posicao_lista((X, Y), [X, Y]).

% -----------------------------------------------------------------------------
% vizinhas(Ilhas, Ilha, Vizinhas)
% Ilhas eh a lista de ilhas de um puzzle, Ilha eh uma dessas ilhas e Vizinhas
% eh a lista ordenada cujos elementos sao as ilhas vizinhas de Ilha.
% -----------------------------------------------------------------------------
vizinhas(Ilhas, Ilha, Vizinhas) :-
    maplist(posicao_ilha, Ilhas, Posicoes),
    maplist(posicao_lista, Posicoes, Lista_Coordenadas), 
    append(Lista_Coordenadas, Lista_Pos),
% Todas as ilhas encontram-se em posicoes entre 0 e XY_Max (em ambos os eixos).
    max_list(Lista_Pos, Max_Coordenada), XY_Max is Max_Coordenada + 1,
    posicao_ilha(Ilha, Pos), Pos = (X, Y), 

    posicoes_entre((0, Y), Pos, Posicoes_1), 
    findall(Ilha_1, (member(Ilha_1, Ilhas), posicao_ilha(Ilha_1, Pos_1), 
    member(Pos_1, Posicoes_1)), Vizinhas_1), % Cima
    posicoes_entre((X, 0), Pos, Posicoes_2), 
    findall(Ilha_2, (member(Ilha_2, Ilhas), posicao_ilha(Ilha_2, Pos_2), 
    member(Pos_2, Posicoes_2)), Vizinhas_2), % Esquerda
    posicoes_entre((X, XY_Max), Pos, Posicoes_3), 
    findall(Ilha_3, (member(Ilha_3, Ilhas), posicao_ilha(Ilha_3, Pos_3), 
    member(Pos_3, Posicoes_3)), Vizinhas_3), % Direita
    posicoes_entre((XY_Max, Y), Pos, Posicoes_4), 
    findall(Ilha_4, (member(Ilha_4, Ilhas), posicao_ilha(Ilha_4, Pos_4), 
    member(Pos_4, Posicoes_4)), Vizinhas_4), % Baixo

    trata_lista_viz(Vizinhas_1, 1, Viz_1), 
    trata_lista_viz(Vizinhas_2, 2, Viz_2), 
    trata_lista_viz(Vizinhas_3, 3, Viz_3), 
    trata_lista_viz(Vizinhas_4, 4, Viz_4),

    append(Viz_1, Viz_2, Lista_Aux_1), append(Viz_3, Viz_4, Lista_Aux_2), 
    append(Lista_Aux_1, Lista_Aux_2, Vizinhas).

% -----------------------------------------------------------------------------
% estado(Ilhas, Estado)
% Ilhas eh a lista de ilhas de um puzzle e Estado eh a lista ordenada cujos
% elementos sao as entradas referentes a cada uma das Ilhas.
% -----------------------------------------------------------------------------
estado(Ilhas, Estado) :-
    findall([Ilha, Ilhas_Vizinhas, []], 
    (member(Ilha, Ilhas), vizinhas(Ilhas, Ilha, Ilhas_Vizinhas)), Estado).

% -----------------------------------------------------------------------------
% posicoes_entre(Pos1, Pos2, Posicoes)
% Posicoes eh a lista ordenada de posicoes entre Pos1 e Pos2. Se Pos1 e Pos2
% nao pertencerem a mesma linha ou a mesma coluna, o resultado eh false.
% -----------------------------------------------------------------------------
posicoes_entre((X1, Y), (X2, Y), Posicoes) :-
    X1 > X2, posicoes_entre((X2, Y), (X1, Y), Posicoes), !.

posicoes_entre((X, Y1), (X, Y2), Posicoes) :-
    Y1 > Y2, posicoes_entre((X, Y2), (X, Y1), Posicoes), !.

posicoes_entre((X1, Y), (X2, Y), Posicoes) :-
    X1_Novo is X1 + 1, X2_Novo is X2 - 1,
    findall((X, Y), between(X1_Novo, X2_Novo, X), Posicoes), !.

posicoes_entre((X, Y1), (X, Y2), Posicoes) :-
    Y1_Novo is Y1 + 1, Y2_Novo is Y2 - 1,
    findall((X, Y), between(Y1_Novo, Y2_Novo, Y), Posicoes), !.

% -----------------------------------------------------------------------------
% cria_ponte(Pos1, Pos2, Ponte)
% Pos1 e Pos2 sao 2 posicoes, e Ponte eh uma ponte entre essas 2 posicoes.
% -----------------------------------------------------------------------------
cria_ponte((X1, Y), (X2, Y), Ponte) :-
    X1 > X2, cria_ponte((X2, Y), (X1, Y), Ponte), !.
    
cria_ponte((X, Y1), (X, Y2), Ponte) :-
    Y1 > Y2, cria_ponte((X, Y2), (X, Y1), Ponte), !.

cria_ponte((X1, Y), (X2, Y), ponte((X1, Y), (X2, Y))).
cria_ponte((X, Y1), (X, Y2), ponte((X, Y1), (X, Y2))).

% -----------------------------------------------------------------------------
% caminho_livre(Pos1, Pos2, Posicoes, I, Vz)
% Posicoes eh a lista ordenada de posicoes entre Pos1 e Pos2, I eh uma ilha e
% Vz eh uma das suas vizinhas. A adicao da ponte ponte(Pos1, Pos2) nao faz com
% que I e Vz deixem de ser vizinhas.
% -----------------------------------------------------------------------------
caminho_livre(Pos1, Pos2, Posicoes, I, Vz) :-
    posicoes_entre(Pos1, Pos2, Posicoes),
    posicao_ilha(I, Pos1), posicao_ilha(Vz, Pos2), !.

caminho_livre(Pos1, Pos2, Posicoes, I, Vz) :-
    posicoes_entre(Pos1, Pos2, Posicoes), 
    posicao_ilha(I, Pos2), posicao_ilha(Vz, Pos1), !.

caminho_livre(Pos1, Pos2, Posicoes, I, Vz) :-
    posicao_ilha(I, Pos_I), posicao_ilha(Vz, Pos_Vz),
    posicoes_entre(Pos_I, Pos_Vz, Posicoes_I_Vz),
    posicoes_entre(Pos1, Pos2, Posicoes),
    intersection(Posicoes_I_Vz, Posicoes, []), !.

% -----------------------------------------------------------------------------
% actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Entrada, Nova_Entrada)
% Posicoes eh a lista ordenada de posicoes entre Pos1 e Pos2. Nova_Entrada eh
% igual a Entrada, excepto no que diz respeito a lista de ilhas vizinhas: esta
% eh actualizada, removendo as ilhas que deixaram de ser vizinhas, apos a
% adicao da ponte.
% -----------------------------------------------------------------------------
actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, [Ilha, Vizinhas, Pontes], 
[Ilha, Nova_Vizinhas, Pontes]) :-
    posicoes_entre(Pos1, Pos2, Posicoes),
    findall(Ilha_Viz, (member(Ilha_Viz, Vizinhas), 
    caminho_livre(Pos1, Pos2, Posicoes, Ilha, Ilha_Viz)), Nova_Vizinhas).

% -----------------------------------------------------------------------------
% actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_estado)
% Pos1 e Pos2 sao as posicoes entre as quais foi adicionada uma ponte, e
% Novo_estado eh o estado que se obtem de Estado apos a actualizacao das ilhas
% vizinhas de cada uma das suas entradas.
% -----------------------------------------------------------------------------
actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_estado) :-
    posicoes_entre(Pos1, Pos2, Posicoes),
    findall(Nova_Entrada, (member(Entrada, Estado), 
    actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Entrada, Nova_Entrada)), 
    Novo_estado).

% -----------------------------------------------------------------------------
% ilhas_terminadas(Estado, Ilhas_term)
% Ilhas_term eh a lista de ilhas que ja tem todas as pontes associadas (ilhas
% terminadas). Se a entrada referente a uma ilha for [ilha(N_pontes, Pos),
% Vizinhas, Pontes], esta ilha esta terminada se N_pontes for diferente de 'X'
% e o comprimento da lista Pontes for N_pontes.
% -----------------------------------------------------------------------------
ilhas_terminadas(Estado, Ilhas_term) :-
    findall(Ilha, (member(Entrada, Estado), 
    Entrada = [ilha(N_Pontes, _), _, Pontes], N_Pontes \== 'X', 
    length(Pontes, N_Pontes), nth1(1, Entrada, Ilha)), Ilhas_term).

% -----------------------------------------------------------------------------
% tira_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_entrada)
% Ilhas_term eh uma lista de ilhas terminadas e Nova_entrada eh a entrada
% resultante de remover as ilhas de Ilhas_term, da lista de ilhas vizinhas de
% Entrada.
% -----------------------------------------------------------------------------
tira_ilhas_terminadas_entrada(Ilhas_term, 
[ilha(N_Pontes, Pos), Vizinhas, Pontes], Nova_entrada) :-
    findall(Vizinha, (member(Vizinha, Vizinhas), 
    \+ member(Vizinha, Ilhas_term)), Nova_Vizinhas),
    Nova_entrada = [ilha(N_Pontes, Pos), Nova_Vizinhas, Pontes].

% -----------------------------------------------------------------------------
% tira_ilhas_terminadas(Estado, Ilhas_term, Novo_estado)
% Ilhas_term eh uma lista de ilhas terminadas e Novo_estado eh o estado
% resultante de aplicar o predicado tira_ilhas_terminadas_entrada a cada uma
% das entradas de Estado.
% -----------------------------------------------------------------------------
tira_ilhas_terminadas(Estado, Ilhas_term, Novo_estado) :-
    findall(Nova_Entrada, (member(Entrada, Estado), 
    tira_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_Entrada)), 
    Novo_estado).

% -----------------------------------------------------------------------------
% marca_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_entrada)
% Ilhas_term eh uma lista de ilhas terminadas e Nova_entrada eh a entrada
% obtida de Entrada da seguinte forma: se a ilha de Entrada pertencer a
% Ilhas_term, o numero de pontes desta eh substituido por 'X'; em caso
% contrario Nova_entrada eh igual a Entrada.
% -----------------------------------------------------------------------------
marca_ilhas_terminadas_entrada(Ilhas_term, [ilha(_, Pos), Vizinhas, Pontes], 
[ilha('X', Pos), Vizinhas, Pontes]) :-
    member(ilha(_, Pos), Ilhas_term), !.

marca_ilhas_terminadas_entrada(_, Entrada, Entrada) :- !.

% -----------------------------------------------------------------------------
% marca_ilhas_terminadas(Estado, Ilhas_term, Novo_estado)
% Ilhas_term eh uma lista de ilhas terminadas e Novo_estado eh o estado
% resultante de aplicar o predicado marca_ilhas_terminadas_entrada a cada uma
% das entradas de Estado.
% -----------------------------------------------------------------------------
marca_ilhas_terminadas(Estado, Ilhas_term, Novo_estado) :-
    findall(Nova_Entrada, (member(Entrada, Estado), 
    marca_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_Entrada)), 
    Novo_estado).

% -----------------------------------------------------------------------------
% trata_ilhas_terminadas(Estado, Novo_estado)
% Novo_estado eh o estado resultante de aplicar os predicados
% tira_ilhas_terminadas e marca_ilhas_terminadas a Estado.
% -----------------------------------------------------------------------------
trata_ilhas_terminadas(Estado, Novo_estado) :-
    ilhas_terminadas(Estado, Ilhas_term),
    marca_ilhas_terminadas(Estado, Ilhas_term, Novo_estado_Aux),
    tira_ilhas_terminadas(Novo_estado_Aux, Ilhas_term, Novo_estado).

% -----------------------------------------------------------------------------
% junta_pontes_entrada(Entrada, Num_pontes, Ilha1, Ilha2, Nova_Entrada)
% Nova_Entrada eh a entrada que se obtem de Entrada por adicao de
% Num_pontes pontes entre Ilha1 e Ilha2.
% -----------------------------------------------------------------------------
junta_pontes_entrada([ilha(N_P, Pos), Vizinhas, Pontes], 1, Ilha1, Ilha2, 
[ilha(N_P, Pos), Vizinhas, Nova_Pontes]) :-
    posicao_ilha(Ilha1, Pos1), posicao_ilha(Ilha2, Pos2), 
    cria_ponte(Pos1, Pos2, Ponte), member(Pos, [Pos1, Pos2]), 
    append(Pontes, [Ponte], Nova_Pontes), !.

junta_pontes_entrada([ilha(N_P, Pos), Vizinhas, Pontes], 2, Ilha1, Ilha2, 
[ilha(N_P, Pos), Vizinhas, Nova_Pontes]) :-
    posicao_ilha(Ilha1, Pos1), posicao_ilha(Ilha2, Pos2), 
    cria_ponte(Pos1, Pos2, Ponte), member(Pos, [Pos1, Pos2]), 
    append(Pontes, [Ponte, Ponte], Nova_Pontes), !.

junta_pontes_entrada(Entrada, _, _, _, Entrada) :- !.

% -----------------------------------------------------------------------------
% junta_pontes(Estado, Num_pontes, Ilha1, Ilha2, Novo_estado)
% Novo_estado eh o estado que se obtem de Estado por adicao de Num_pontes
% pontes entre Ilha1 e Ilha2.
% -----------------------------------------------------------------------------
junta_pontes(Estado, Num_pontes, Ilha1, Ilha2, Novo_estado) :-
    posicao_ilha(Ilha1, Pos1), posicao_ilha(Ilha2, Pos2),
    findall(Nova_Entrada, (member(Entrada, Estado), 
    junta_pontes_entrada(Entrada, Num_pontes, Ilha1, Ilha2, Nova_Entrada)), 
    Novo_estado_Aux),
    actualiza_vizinhas_apos_pontes(Novo_estado_Aux, Pos1, Pos2, 
    Novo_estado_Aux_2),
    trata_ilhas_terminadas(Novo_estado_Aux_2, Novo_estado).
