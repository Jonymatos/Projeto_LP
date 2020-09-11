%Nome Joao Rui Vargas de Matos Numero 95610

:- [codigo_comum].

%pertence(EL,Lst)
%Predicado auxiliar que devolve True se EL e identico pelo menos um elemento da lista Lst
pertence(P, [Q | _]) :- P == Q ,!.
pertence(P, [_ | R]) :- pertence(P, R).


%obtem_letras_palavras_aux(Lst_Pals, Letras)
%Predicado auxiliar em que Lst_Pals e uma lista de palavras ja ordenada alfabeticamente,  
%significa que Letras e a lista cujos elementos sao listas com as letras de cada palavra de Lst_Pals
obtem_letras_palavras_aux([],[]).
obtem_letras_palavras_aux([Pal|Lst_pals_Ord],[Pal_letras|Letras]) :- 
    atom_chars(Pal,Pal_letras),
    obtem_letras_palavras_aux(Lst_pals_Ord,Letras).
    
%obtem_letras_palavras(Lst_Pals, Letras)
%Em que Lst_Pals e uma lista de palavras, significa que Letras e a lista ordenada cujos elementos 
%sao listas com as letras de cada palavra de Lst_Pals
obtem_letras_palavras(Lst_pals,Letras) :- 
    sort(Lst_pals,Lst_Ord),
    obtem_letras_palavras_aux(Lst_Ord,Letras).


%espaco_fila(Fila, Esp)
%Em que Fila e uma fila (linha ou coluna) de uma grelha,significa que Esp e um espaco de Fila
espaco_fila(Fila,Espaco):- espaco_fila(Fila,Espaco,[],[]).

espaco_fila([],Espaco,Ac,List_Esp) :-
    length(Ac,Comp),Comp > 2,
    append(List_Esp,[Ac],Lists),
    espaco_fila([],Espaco,[],Lists).

espaco_fila([],Espaco,_,List_Esp) :-
    append(List_Esp,Espaco),
    member(Espaco,List_Esp).
    %O Espaco tem de ser uma concatenacao de List_Esp e membro de List_Esp,
    %para garantir que o predicado nao devolva solucoes a mais
    
espaco_fila([EL|Fila],Espaco,Ac,List_Esp) :-
    EL =='#',
    length(Ac,Comp),Comp > 2,
    append(List_Esp,[Ac],Lists),
    espaco_fila(Fila,Espaco,[],Lists).

espaco_fila([EL|Fila],Espaco,_,List_Esp) :-
    EL =='#',!,
    espaco_fila(Fila,Espaco,[],List_Esp).

espaco_fila([EL|Fila],Espaco,Ac,List_Esp) :-
    append(Ac,[EL],Esp),
    espaco_fila(Fila,Espaco,Esp,List_Esp).


%espacos_fila(Fila, Espacos)
%Em que Fila e uma fila (linha ou coluna) de umagrelha, significa que Espacos e a lista de todos
%os espacos de Fila, da esquerda para a direita.
espacos_fila(Fila,[]):- \+espaco_fila(Fila,_),!.
%Se nao houver espacos e retornada uma lista vazia

espacos_fila(Fila,Espacos):-
    bagof(Espaco, espaco_fila(Fila,Espaco),Espacos).


%espacos_puzzle(Grelha,Espacos)
%Em que Grelha e uma grelha, significa que Espacos e a lista de espacos de Grelha
espacos_puzzle(Grelha,Espacos) :- 
    mat_transposta(Grelha,Matriz),
    append(Grelha,Matriz,NovaGrelha),
    espacos_puzzle(NovaGrelha,Espacos,[]).

espacos_puzzle([],Espacos,Espacos).

espacos_puzzle([Fila|Grelha],Espacos,Ac) :-
    espacos_fila(Fila,Espaco),
    append(Ac,Espaco,NovoAc),
    espacos_puzzle(Grelha,Espacos,NovoAc).


%posicoes_identicas(Esp1,Esp2)
%Predicado auxiliar que devolve True se os dois espacos tiverem pelo menos 
%um elemento identico entre si
posicoes_identicas([EL|_],L2) :-
    pertence(EL,L2),!.

posicoes_identicas([_|L1],L2) :-
    posicoes_identicas(L1,L2).

%espacos_com_posicoes_comuns(Espacos, Esp, Esps_com)
%Em que Espacos e uma lista de espacos e Esp e um espaco, significa que Esps_com e a
%lista de espacos com variaveis em comum com Esp, exceptuando Esp. Os espacos em 
%Esps_com devem aparecer pela mesma ordem que aparecem em Esps
espacos_com_posicoes_comuns([],_,[]).
espacos_com_posicoes_comuns([EL|Espacos],Esp,Esp_com) :-
    EL == Esp,!,
    espacos_com_posicoes_comuns(Espacos,Esp,Esp_com).

espacos_com_posicoes_comuns([Espaco|Espacos],Esp,[Espaco|Esp_com]) :-
    posicoes_identicas(Espaco,Esp),!,
    espacos_com_posicoes_comuns(Espacos,Esp,Esp_com).

espacos_com_posicoes_comuns([_|Espacos],Esp,Esp_com):-
    espacos_com_posicoes_comuns(Espacos,Esp,Esp_com).


%eh_coincidivel(Esp,Pal)
%Predicado auxiliar que verifica se Esp e Pal sao unificaveis entre si sem  
%alterar o Esp, ou seja, nao fazer as unificacoes
eh_coincidivel(Esp,Pal) :-
    copy_esp(Esp,Copy),
    Copy=Pal.

copy_el(EL,_):- 
    var(EL),!.

copy_el(EL,EL).

copy_esp(Esp,Copy):- 
    maplist(copy_el,Esp,Copy).

%palavra_possivel_esp(Pal, Esp, Espacos, Letras)
%Em que Pal e uma lista de letras de uma palavra, Esp e um espaco, Espacos e uma 
%lista de espacos, e Letras euma lista de listas de letras de palavras, 
%significa que Pal e uma palavra possivel parao espaco Esp
palavra_possivel_esp(Pal, Esp, Espacos, Letras) :-
    member(Pal,Letras),
    espacos_com_posicoes_comuns(Espacos,Esp,Esp_com),
    eh_coincidivel(Esp,Pal),
    Pal=Esp,
    %Faz a unificacao para que o Esp_com, tenha o(s) espaco(s) actualizado(s)
    palavras_possiveis_esp_aux(Letras,Esp_com).

%palavras_possiveis_esp_aux(Letras,Esp_com)
%Predicado auxiliar em que todos os Esp_com sao unificaveis pelo menosuma das Letras
palavras_possiveis_esp_aux(_,[]):-!.
palavras_possiveis_esp_aux(Letras,[Esp|Espacos]):-
    testa_palavras(Esp,Letras),
    palavras_possiveis_esp_aux(Letras,Espacos).

%testa_palavras(Esp_com,Pal_letras)
%Predicado auxiliar em que Esp_com e unificavel pelo menos uma das Pal_letras
testa_palavras(Esp_com, [Pal|_]) :- eh_coincidivel(Esp_com,Pal),!.
testa_palavras(Esp_com,[_|Pal_letras]) :- testa_palavras(Esp_com,Pal_letras).


%palavras_possiveis_esp(Letras, Espacos, Esp, Pals_Possiveis)
%Em que Letras e uma lista de listas de letras de palavras, Espacos e uma lista de espacos,
%Esp e um espaco, significa que Pals_Possiveis e a lista ordenada de palavras
%possiveis para o espaco Esp
palavras_possiveis_esp(Letras, Espacos, Esp, Pals_Possiveis) :-
    findall(Pal, palavra_possivel_esp(Pal, Esp, Espacos, Letras),Pals_Possiveis).


%palavras_possiveis(Letras, Espacos, Pals_Possiveis)
%Em que Letras e uma lista de listas de letras de palavras e 
%Espacos e uma lista de espacos, significa que
%Pals_Possiveis e a lista de palavras possiveis
palavras_possiveis(Letras, Espacos, Pals_Possiveis) :- 
    palavras_possiveis(Letras, Espacos, Pals_Possiveis, Espacos).
    
palavras_possiveis(_,[],[],_).
palavras_possiveis(Letras, [Esp|Espacos], [Pal_Possivel|Pals_Possiveis],Todos_Espacos) :-
    Pal_Possivel=[Esp|[Pals]],
    palavras_possiveis_esp(Letras,Todos_Espacos,Esp,Pals),
    palavras_possiveis(Letras,Espacos,Pals_Possiveis,Todos_Espacos).


%letras_comuns(Lst_Pals, Letras_comuns)
%Em que Lst_Pals e uma lista delistas de letras, significa que Letras_comuns 
%e uma lista de pares(pos, letra), significando que todas as listas de 
%Lst_Pals contem a letra letra na posicao pos 
letras_comuns([Pal|Lst_Pals], Letras_comuns) :- 
    length(Pal,Comp),
    Ref is Comp + 1,
    letras_comuns([Pal|Lst_Pals], Letras_comuns, 1, Ref).
    %Ref usado para garantir que o predicado so devolva uma unica solucao
    %quando acabar de explorar todas as letras de Lst_Pals

letras_comuns(_,[],Ref,Ref):-!.
letras_comuns(Lst_Pals, [Par_Letras|Letras_comuns], Indice, Ref) :-
    letras_iguais(Lst_Pals,Indice,Letra),!,
    Par_Letras=(Indice,Letra),
    N_Indice is Indice + 1,
    letras_comuns(Lst_Pals,Letras_comuns, N_Indice, Ref).

letras_comuns(Lst_Pals,Letras_comuns,Indice,Ref):-
    N_Indice is Indice + 1,
    letras_comuns(Lst_Pals,Letras_comuns,N_Indice,Ref).

%letras_iguais(Lst_Pals,Indice,Letra)
%Predicado auxiliar em que a Letra e comum a todas as palavras de 
%Lst_Pals num determinado indice
letras_iguais([],_,_).
letras_iguais([Pal|Lst_Pals],Indice,Letra) :-
    nth1(Indice,Pal,Letra),
    letras_iguais(Lst_Pals, Indice, Letra).


%atribui_comuns(Pals_Possiveis)
%Em que Pals_Possiveis e uma lista de palavras possiveis, actualiza esta lista atribuindo
%a cada espaco as letras comuns a todas as palavras possiveis para esse espaco
atribui_comuns([]).
atribui_comuns([Pal|Pals_Possiveis]):-
    Pal=[Esp|[Letras]],
    letras_comuns(Letras,Lst_com),
    atribui_comuns_aux(Esp,Lst_com),
    atribui_comuns(Pals_Possiveis).

%atribui_comuns_aux(Esp,Lst_com)
%Predicado auxiliar em que actualiza o Esp de acordo com o Lst_com
atribui_comuns_aux(_,[]):-!.
atribui_comuns_aux(Esp,[Par|Lst_com]):-
    Par=(Indice,Letra),
    nth1(Indice,Esp,Pos),
    Pos=Letra,
    atribui_comuns_aux(Esp,Lst_com).


%retira_impossiveis(Pals,Novas_Pals)
%Em que Pals e uma lista de palavras possiveis, significa que
%Novas_Pals e o resultado de tirar palavras impossiveis de Pals
retira_impossiveis([],[]).
retira_impossiveis([Pal|Pals],[Nova_Pal|Novas_Pals]) :-
    Pal=[Esp|[Lst_Pals]],
    retira_impossiveis_aux(Esp,Lst_Pals,Pals_filt),
    append([Esp],[Pals_filt],Nova_Pal),
    retira_impossiveis(Pals,Novas_Pals).

%retira_impossiveis_aux(Esp,Pals,Pals_filt)
%Predicado auxiliar em que Pals_filt e o resultado de retirar palavras impossiveis
%de Pals de um determinado espaco
retira_impossiveis_aux(Esp,Pals,Pals_filt) :- retira_impossiveis_aux(Esp,Pals,Pals_filt,[]).

retira_impossiveis_aux(_,[],Pals,Pals):-!.
retira_impossiveis_aux(Esp,[Pal|Pals],Pals_filt,Ac):-
    eh_coincidivel(Esp,Pal),!,
    append(Ac,[Pal],NovoAc),
    retira_impossiveis_aux(Esp,Pals,Pals_filt,NovoAc).

retira_impossiveis_aux(Esp,[_|Pals],Pals_filt,Ac):-
    retira_impossiveis_aux(Esp,Pals,Pals_filt,Ac).


%obtem_unicas(Pals_Possiveis, Unicas)
%Em que Pals_Possiveis e uma lista de palavras possiveis, significa 
%que Unicas e a lista de palavras unicas de Pals_Possiveis
obtem_unicas(Pals_Possiveis, Unicas) :- obtem_unicas(Pals_Possiveis,Unicas,[]).

obtem_unicas([],Unicas,Unicas).
obtem_unicas([Pal|Pals_Possiveis],Unicas,Ac):-
    Pal=[Esp|[[Pal_letra]]],
    eh_coincidivel(Esp,Pal_letra),!,
    append(Ac,[Pal_letra],NovoAc),
    obtem_unicas(Pals_Possiveis,Unicas,NovoAc).

obtem_unicas([_|Pals_Possiveis],Unicas,Ac):-
    obtem_unicas(Pals_Possiveis,Unicas,Ac).


%retira_unicas_aux(Lst_Pals,Nova_Lst_Pals,Unicas)
%Predicado auxiliar em que Nova_Lst_Pals e o resultado de retirar 
%palavras unicas de Lst_Pals de acordo com a lista Unicas
retira_unicas_aux(Pals,Pals,_) :-
    length(Pals,Comp),Comp == 1,!.

retira_unicas_aux(Pals,Novas_Pals,Unicas) :- retira_unicas_aux(Pals,Novas_Pals,Unicas,[]).

retira_unicas_aux([],Pals,_,Pals):-!.
retira_unicas_aux([Pal|Pals],Novas_Pals,Unicas,Ac):-
    \+pertence(Pal,Unicas),!,
    append(Ac,[Pal],NovoAc),
    retira_unicas_aux(Pals,Novas_Pals,Unicas,NovoAc).

retira_unicas_aux([_|Pals],Novas_Pals,Unicas,Ac):-
    retira_unicas_aux(Pals,Novas_Pals,Unicas,Ac).

%retira_unicas(Pals_Possiveis,Novas_Pals)
%Em que Pals_Possiveis e uma lista de palavras possiveis, significa que
%Novas_Pals e o resultado de retirar de Pals_Possiveis as palavras unicas
retira_unicas(Pals_Possiveis,Novas_Pals):- 
    obtem_unicas(Pals_Possiveis,Unicas),
    retira_unicas(Pals_Possiveis,Novas_Pals,Unicas).

retira_unicas([],[],_).
retira_unicas([Pal|Pals],[Nova_Pal|Novas_Pals],Unicas) :-
    Pal=[Esp|[Lst_Pals]],
    retira_unicas_aux(Lst_Pals,Nova_Lst_Pals,Unicas),
    append([Esp],[Nova_Lst_Pals],Nova_Pal),
    retira_unicas(Pals,Novas_Pals,Unicas).


%simplifica(Palavras_Possiveis,Novas_Palavras_Possiveis)
%Em que Palavras_Possiveis e uma lista de palavras possiveis, significa que
%Novas_Palavras_Possiveis e o resultado de simplificar Pals_Possiveis
simplifica(Palavras_Possiveis,Novas_Palavras_Possiveis) :-
    %Enquanto Palavras_Possiveis e Novas_Palavras_Possiveis nao forem identicas
    atribui_comuns(Palavras_Possiveis),
    retira_impossiveis(Palavras_Possiveis,Palavras_Retiradas_Possiveis),
    retira_unicas(Palavras_Retiradas_Possiveis,Novas_Palavras_Retiradas),
    Palavras_Possiveis \== Novas_Palavras_Retiradas,!,
    simplifica(Novas_Palavras_Retiradas,Novas_Palavras_Possiveis).

simplifica(Palavras_ja_Simplificadas,Palavras_ja_Simplificadas) :-
    atribui_comuns(Palavras_ja_Simplificadas),!.


%inicializa(Puzzle,Palavras_Possiveis)
%Em que Puzzle e um puzzle, significa que Pals_Possiveis e a 
%lista de palavras possiveis simplificada para Puzzle.
inicializa(Puzzle,Palavras_Possiveis) :-
    Puzzle=[Palavras|[Grelha]],
    obtem_letras_palavras(Palavras,Letras),
    espacos_puzzle(Grelha, Espacos),
    palavras_possiveis(Letras, Espacos, Palavras_por_Atribuir),
    simplifica(Palavras_por_Atribuir,Palavras_Possiveis).


%filtra_palavras(Palavras,Palavras_filt)
%Predicado auxiliar em que Palavras_filt tem apenas os espacos
%de Palavras que contem pelo menos mais do que 2 palavras
filtra_palavras(Palavras,Palavras_filt) :-
    filtra_palavras(Palavras,Palavras_filt,[]).

filtra_palavras([],Palavras,Palavras).
filtra_palavras([Pal|Palavras],Palavras_filt,Ac) :-
    Pal=[_|[Lst_Pals]],
    length(Lst_Pals,Comp),
    Comp >= 2,!,
    append(Ac,[Pal],NovoAc),
    filtra_palavras(Palavras,Palavras_filt,NovoAc).

filtra_palavras([_|Palavras],Palavras_filt,Ac) :-
    filtra_palavras(Palavras,Palavras_filt,Ac).

%devolve_escolha(Pals,Esc)
%Predicado auxiliar que devolve o espaco (o Esc) que contem 
%menos palavras tendo em conta A lista Pals
devolve_escolha([],_):-!,fail.
devolve_escolha([Pal|Palavras],Esc) :- 
    devolve_escolha(Palavras,Esc,Pal).

devolve_escolha([],Esc,Esc).
devolve_escolha([Pal|Palavras],Esc, Reserva) :-
    Pal=[_|[Lst_Pals]],
    length(Lst_Pals,Comp_Lst),
    Reserva=[_|[Lst_Res]],
    length(Lst_Res,Comp_Reserva),
    Comp_Lst < Comp_Reserva,!,
    devolve_escolha(Palavras,Esc,Pal).

devolve_escolha([_|Palavras],Esc,Reserva):-
    devolve_escolha(Palavras,Esc,Reserva).

%escolhe_menos_alternativas(Palavras, Escolha)
%Em que Palavras e uma lista de palavras possiveis, significa que Escolha e o
%elemento de Palavras, se todos os espacos em Pals_Possiveis tiverem associadas 
%listas de palavras unitarias, o predicado devolve false.
escolhe_menos_alternativas(Palavras, Escolha) :-
    filtra_palavras(Palavras, Palavras_filt),
    devolve_escolha(Palavras_filt, Escolha).


%experimenta_esp(Pal,Nova_Pal)
%Predicado auxiliar em que Nova_Pal e o resultado de experimentar as palavras de Pal
experimenta_esp(Pal,Nova_Pal):-
    Pal=[Esc|[Pals]],
    member(Esc,Pals),
    Nova_Pal=[Esc|[[Esc]]].

%experimenta_pal(Escolha,Palavras,Novas_Palavras)
%Em que Palavras e uma lista de palavras possiveis, e Escolha e um dos seus elementos 
%escolhido por escolhe_menos_alternativas/2)Novas_Palavras e o resultado de substituir
%em Palavras, o elemento Escolha pelo elemento
experimenta_pal(Escolha,Palavras,Novas_Palavras):- 
    escolhe_menos_alternativas(Palavras,Escolha),
    experimenta_pal(Escolha,Palavras,Novas_Palavras,_).

experimenta_pal(_,[],[],_).
experimenta_pal(Escolha,[Pal|Palavras],[Nova_Pal|Novas_Palavras],_):-
    Escolha==Pal,!,
    experimenta_esp(Escolha,Nova_Pal),
    experimenta_pal(Escolha,Palavras,Novas_Palavras,_).

experimenta_pal(Escolha,[Pal|Palavras],[Pal|Novas_Palavras],_):-
    experimenta_pal(Escolha,Palavras,Novas_Palavras,_).


%resolve_aux(Palavras,Novas_Palavras)
%Em que Palavras e uma lista de palavras possiveis, significa que
%Novas_Palavras e o resultado de escolher espaco, experimentar uma
%palavras ate estar simplificado
resolve_aux(Palavras,Novas_Palavras) :- 
    %Enquanto houver escolhas...
    escolhe_menos_alternativas(Palavras,Escolha),!,
    experimenta_pal(Escolha,Palavras,Exp_Palavras),
    simplifica(Exp_Palavras,Palavras_Simp),
    resolve_aux(Palavras_Simp,Novas_Palavras).

resolve_aux(Palavras_Res,Palavras_Res):-
    simplifica(Palavras_Res,Palavras_Res).


%resolve(Puzzle)
%Em que Puzzle e um puzzle, resolve esse puzzle, isto e, apos a invocacao deste predicado a grelha de Puzzle
%tem todas as variaveis substituidas por letras que constituem as palavras da lista de palavras de Puzzle.
resolve(Puzzle):-
    inicializa(Puzzle,Palavras_Possiveis),
    resolve_aux(Palavras_Possiveis,Palavras_Res),
    simplifica(Palavras_Res,Palavras_Res).