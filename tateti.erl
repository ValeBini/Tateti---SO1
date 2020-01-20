-module(tateti).
-compile(export_all).

vacio() ->
    [["-","-","-"],
     ["-","-","-"],
     ["-","-","-"]].

pos(X, Y, Tablero, J) ->
    if (X<1) or (X>3) or (Y<1) or (Y>3) -> Tablero;
       true ->
            case lists:nth(Y, lists:nth(X, Tablero)) of
                "-" -> lists:sublist(Tablero,X-1)
                        ++ [lists:sublist(lists:nth(X,Tablero), Y-1) ++ [J] ++ lists:nthtail(Y, lists:nth(X,Tablero))]
                        ++ lists:nthtail(X, Tablero);

                _ -> Tablero
            end
    end.

prettifyrow(List) ->
    " "++lists:nth(1, List)++" | "++lists:nth(2, List)++" | "++lists:nth(3, List)++" ".


prettifyboard(List) ->
    "   |   |   \n"++
    prettifyrow(lists:nth(1, List))
    ++"\n"++"___|___|___\n   |   |   \n"++
    prettifyrow(lists:nth(2, List))
    ++"\n"++"___|___|___\n   |   |   \n"++
    prettifyrow(lists:nth(3, List))
    ++"\n   |   |   ".


gano([[V,_,_],[_,V,_],[_,_,V]], V) -> true;
gano([[_,_,V],[_,V,_],[V,_,_]], V) -> true;
gano([[V,_,_],[V,_,_],[V,_,_]], V) -> true;
gano([[_,V,_],[_,V,_],[_,V,_]], V) -> true;
gano([[_,_,V],[_,_,V],[_,_,V]], V) -> true;
gano([[V,V,V],[_,_,_],[_,_,_]], V) -> true;
gano([[_,_,_],[V,V,V],[_,_,_]], V) -> true;
gano([[_,_,_],[_,_,_],[V,V,V]], V) -> true;
gano(_,_) -> false.


lleno(T) ->
    not(lists:foldl(fun(E,Or) -> E or Or end, false, lists:map(fun (F) -> lists:member("-", F) end, T))).

countLetter(Tablero, C) ->
    lists:foldl(fun(S,Sum) -> S+Sum end,
                0,
                lists:map(fun(L) -> lists:foldl(fun (F, Sum) -> F+Sum end,
                                    0,
                                    lists:map(fun(E) -> if E==C -> 1;
                                                           true -> 0
                                                        end
                                              end, L)) end, Tablero)).
