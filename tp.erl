-module(test).
-define(SERVERS, ['nodo1@192.168.1.198','nodo2@192.168.1.160']).
-compile(export_all).
-import(tateti, [vacio/0, gano/2, pos/4, prettifyboard/1, countLetter/2, lleno/1]).

play(Port_to_listen) ->
    N = lists:last(lists:nth(1,string:tokens(atom_to_list(node()), "@"))),
    Servers = lists:delete(node(), ?SERVERS),
    register(pstatRegister, spawn(node(), ?MODULE, pStat, [Servers])),
    register(pbalanceRegister, spawn(node(), ?MODULE, pBalance, [[]])),
    PidMoves = spawn(node(),?MODULE, movesList, [integer_to_list(N),0]),
    register(movesRegister, PidMoves),
    PidUsers = spawn(node(),?MODULE, usersList, [[],Servers]),
    register(usersRegister, PidUsers),
    PidGames = spawn(node(),?MODULE, gamesList, [[],Servers]),
    register(gamesRegister, PidGames),
    PidDisp = spawn(node(),?MODULE, dispatcher, [Port_to_listen,PidUsers, PidGames, PidMoves]).

pStat(Servers) ->
    Load = erlang:statistics(total_active_tasks),
    lists:foreach(fun(S) -> {pbalanceRegister, S}!{node(), Load} end, Servers),
    timer:sleep(100),
    pStat(Servers).

pBalance(Loads) ->
    receive
        {Pid, request} -> {Node, Load} = lists:nth(1, Loads),
                          Pid!Node,
                          receive {Pid,ok} -> NewOrderedList = Loads end;
        {Node, Load} -> NewList = lists:keystore(Node, 1, Loads, {Node, Load}),
                        NewOrderedList = lists:keysort(2, NewList)
    end,
    pBalance(NewOrderedList).

dispatcher(Port_to_listen, PidUsers, PidGames, PidMoves) ->
 {ok, ListenSocket} = gen_tcp:listen(Port_to_listen, [binary, {active, false}]),
 wait_connect(ListenSocket,0, PidUsers, PidGames,PidMoves).

wait_connect(ListenSocket, Count, PidUsers, PidGames, PidMoves) ->
 {ok, Socket} = gen_tcp:accept(ListenSocket),
 PidUp = spawn(?MODULE, updSocket, [Socket]),
 spawn(?MODULE, pSocket, [Socket, Socket, PidUp, Count, PidUsers, PidGames, PidMoves]),
 wait_connect(ListenSocket, Count+1, PidUsers, PidGames, PidMoves).


pSocket(Socket, Username, PidUp, Count, PidUsers, PidGames, PidMoves) ->
 case gen_tcp:recv(Socket, 0) of
 {ok, CMD} -> pbalanceRegister!{self(), request},
              receive
                   Node -> spawn(Node, ?MODULE, pComando, [Socket, Username, binary_to_list(CMD), self(), PidUp, PidUsers, PidGames, PidMoves]),
                           pbalanceRegister!{self(),ok}
              end;
 {error, M} -> io:format("Se ha producido un error en el cliente ~p, conexion cerrada.\n", [Count])
 end,
 receive
     {error, Msg} -> gen_tcp:send(Socket, Msg),
                     pSocket(Socket, Username, PidUp, Count, PidUsers, PidGames, PidMoves);
     {con, error, Msg} -> gen_tcp:send(Socket, Msg),
                          pSocket(Socket, Username, PidUp, Count, PidUsers, PidGames, PidMoves);
     {con, ok, Nombre} -> gen_tcp:send(Socket, "Conexion exitosa\n"),
                          pSocket(Socket, Nombre, PidUp, Count, PidUsers, PidGames, PidMoves);
     {lsg, Lista} -> gen_tcp:send(Socket, Lista),
                     pSocket(Socket, Username, PidUp, Count, PidUsers, PidGames, PidMoves);
     {new, ok, Id} -> gen_tcp:send(Socket, "Nuevo juego numero " ++ Id ++ " creado\n"),
                      pSocket(Socket, Username, PidUp, Count, PidUsers, PidGames, PidMoves);
     {acc, error, Msg} -> gen_tcp:send(Socket, Msg),
                          pSocket(Socket, Username, PidUp, Count, PidUsers, PidGames, PidMoves);
     {acc, ok, Id} -> gen_tcp:send(Socket, "Usted ha ingresado al juego " ++ Id ++ "\n"),
                      pSocket(Socket, Username, PidUp, Count, PidUsers, PidGames, PidMoves);
     {obs, error, Msg} -> gen_tcp:send(Socket, Msg),
                          pSocket(Socket, Username, PidUp, Count, PidUsers, PidGames, PidMoves);
     {obs, ok, Id} -> gen_tcp:send(Socket, "Usted ha ingresado al juego " ++ Id ++ " como observador\n"),
                      pSocket(Socket, Username, PidUp, Count, PidUsers, PidGames, PidMoves);
     {pla, error, Msg} -> gen_tcp:send(Socket, Msg),
                          pSocket(Socket, Username, PidUp, Count, PidUsers, PidGames, PidMoves);
     {pla, ok, T, Msg} -> gen_tcp:send(Socket, "OK \n" ++ T ++ "\n" ++ Msg),
                          pSocket(Socket, Username, PidUp, Count, PidUsers, PidGames, PidMoves);
     {out, error, Msg} -> gen_tcp:send(Socket, Msg),
                          pSocket(Socket, Username, PidUp, Count, PidUsers, PidGames, PidMoves);
     {out, ok} -> gen_tcp:send(Socket, "Ha salido del juego.\n"),
                  pSocket(Socket, Username, PidUp, Count, PidUsers, PidGames, PidMoves);
     {lea, ok} -> gen_tcp:send(Socket, "Ha salido del juego.\n"),
                  pSocket(Socket, Username, PidUp, Count, PidUsers, PidGames, PidMoves);
     {lea, error, Msg} -> gen_tcp:send(Socket, Msg),
                          pSocket(Socket, Username, PidUp, Count, PidUsers, PidGames, PidMoves);
     {bye, ok} -> gen_tcp:send(Socket, "Usted se ha desconectado.\n");
     {ok, ok} -> pSocket(Socket, Username, PidUp, Count, PidUsers, PidGames, PidMoves)
 end.

updSocket(Socket) ->
    receive
        {upd, Cmdid, Msg} -> gen_tcp:send(Socket, "UPD " ++ Cmdid ++ " " ++ Msg)
    end,
    updSocket(Socket).


pComando(Socket, Username, CMD, Pid, PidUp, PidUsers, PidGames, PidMoves) ->
 Pedido = string:tokens(string:strip(string:strip(CMD, right, $\n),right,$\r), " "),
 Fst = lists:nth(1,Pedido),
 if ((Socket==Username) and ( Fst /= "CON")) -> Pid!{error, "Debe conectarse y crear un nombre de usuario para realizar cualquier otra acción\n"};
    true -> (case Pedido of
                ["CON", Nombre] -> if Socket == Username -> Pid!conectar(Socket, Nombre, PidUsers);
                                      true -> Pid!{con, error, "Ya has elegido un nombre de usuario\n"}
                                   end;
                ["LSG"] -> Pid!listarJuegos(Socket, PidGames);
                ["NEW"] -> Pid!crearJuego(Socket, PidUp, Username, PidGames);
                ["ACC", JuegoId] -> Pid!aceptarJuego(Socket, PidUp, Username, JuegoId, PidGames);
                ["OBS", JuegoId] -> Pid!añadirObs(Socket, PidUp, Username, JuegoId, PidGames);
                ["PLA", JuegoId, "OUT"] -> Pid!salirDelJuego(Socket, JuegoId, Username, PidGames, PidMoves);
                ["PLA", JuegoId, "PUT", X, Y] -> Pid!realizarJugada(Socket, JuegoId, Username, list_to_integer(X), list_to_integer(Y), PidGames, PidMoves);
                ["LEA", JuegoId] -> Pid!eliminarObs(Socket, JuegoId, PidGames);
                ["BYE"] -> Pid!desconectar(Socket, Username, PidUsers, PidGames, PidMoves);
                ["OK", Cmdid] -> Pid!{ok, ok};
                Otherwise -> Pid!{error, "Comando invalido\n"}
            end)
end.

distribute(List, {N1, L1}, {N2, L2}) ->
    if N1 /= 0 -> List1 = lists:sublist(List, L1),
                  List2 = lists:nthtail(L1, List),
                  [List1 | distribute(List2, {N1-1, L1}, {N2, L2})];
       N2 /= 0 -> List1 = lists:sublist(List, L2),
                  List2 = lists:nthtail(L2, List),
                  [List1 | distribute(List2, {N1,L1}, {N2-1, L2})];
       true -> []
    end.

calc_load(NL, NS) ->
    Result = trunc(NL/NS),
    if Result == 0 -> {{NL, 1}, {NS-NL, 0}};
        true -> Resto = NL rem NS,
                {{NS-Resto, Result}, {Resto, Result+1}}
    end.



usersList(Users, Servers) ->
    io:format("jugadores: ~p\n", [Users]),
    receive
        {Pid, users, justme} -> io:format("pedido recibido\n"),
                                Pid!{node(), Users},
                                io:format("enviado\n"),
                                receive {Pid, NewList, justme} -> io:format("nueva lista: ~p\n", [NewList]),
                                                                  Pid!{node(),ok} end;
        {Pid, users, all} -> SendList = lists:map(fun(S) -> {usersRegister, S} end, Servers),
                             lists:foreach(fun(P) -> P!{self(), users, justme} end, SendList),
                             UsersLists = lists:map(fun(S) -> receive {S,  List} -> List end end, Servers),
                             io:format("UsersList: ~p\n", [UsersLists]),
                             List_to_send = Users ++ lists:concat(UsersLists),
                             io:format("List_to_send: ~p\n", [List_to_send]),
                             Pid!List_to_send,
                             receive {Pid, ListAct, all} -> NS = length(?SERVERS),
                                                            NL = length(ListAct),
                                                            {T1,T2} = calc_load(NL, NS),
                                                            [NewList | Lists_to_return] = distribute(ListAct, T1, T2),
                                                            lists:foreach(fun({P,L}) -> P!{self(), L, justme} end, lists:zip(SendList, Lists_to_return)),
                                                            lists:foreach(fun(S) -> receive {S, ok} -> ok end end, Servers),
                                                            Pid!ok
                             end
    end,
    usersList(NewList, Servers).

gamesList(Games, Servers) ->
    io:format("jugadores: ~p\n", [Games]),
    receive
        {Pid, games, justme} -> io:format("pedido recibido\n"),
                                Pid!{node(), Games},
                                io:format("enviado\n"),
                                receive {Pid, NewList, justme} -> io:format("nueva lista: ~p\n", [NewList]),
                                                                  Pid!{node(),ok} end;
        {Pid, games, all} -> SendList = lists:map(fun(S) -> {gamesRegister, S} end, Servers),
                             lists:foreach(fun(P) -> P!{self(), games, justme} end, SendList),
                             GamesLists = lists:map(fun(S) -> receive {S,  List} -> List end end, Servers),
                             io:format("GamesList: ~p\n", [GamesLists]),
                             List_to_send = lists:keysort(1, Games ++ lists:concat(GamesLists)),
                             io:format("List_to_send: ~p\n", [List_to_send]),
                             Pid!List_to_send,
                             receive {Pid, ListAct, all} -> NS = length(?SERVERS),
                                                            NL = length(ListAct),
                                                            {T1,T2} = calc_load(NL, NS),
                                                            [NewList | Lists_to_return] = distribute(ListAct, T1, T2),
                                                            lists:foreach(fun({P,L}) -> P!{self(), L, justme} end, lists:zip(SendList, Lists_to_return)),
                                                            lists:foreach(fun(S) -> receive {S, ok} -> ok end end, Servers),
                                                            Pid!ok
                             end
    end,
    gamesList(NewList, Servers).


movesList(N, NextMove) ->
    receive {Pid, move} -> StrNextMove = integer_to_list(NextMove),
                           Pid!(N++StrNextMove) end,
    receive {Pid, ok} -> ok end,
    movesList (N, NextMove+1).

conectar(Socket, Nombre, PidUsers) ->
    PidUsers!{self(), users, all},
    receive
        Users -> B = lists:member(Nombre, Users),
                 if B -> PidUsers!{self(),Users,all},
                         io:format("lista nueva enviado\n"),
                         receive ok -> {con, error, "Nombre ocupado\n"} end;
                    true -> PidUsers!{self(), [Nombre | Users], all},
                            io:format("lists nueva enviada\n"),
                            receive ok -> {con, ok, Nombre} end
                 end
    end.

listarJuegos(Socket, PidGames) ->
    PidGames!{self(), games, all},
    receive
        Games -> Str = lists:concat(lists:map(fun({Id, {U1, P1}, {U2, P2}, T, Obs}) -> if U2==nul -> Id++" "++U1++" en espera...\n";
                                                                                           true -> Id++" "++U1++" "++U2++"\n"
                                                                                        end
                                               end, Games)),
                  PidGames!{self(),Games, all},
                  receive ok ->  {lsg, Str} end
    end.

crearJuego(Socket, PidUp, Username, PidGames) ->
    PidGames!{self(), games, all},
    receive
        Games -> case Games of
                    [] -> Id = integer_to_list(1);
                    XS -> {LastId, _, _, _, _} = lists:last(XS),
                          Id = integer_to_list(list_to_integer(LastId)+1)
                 end,
                 NewGame = {Id, {Username, PidUp}, {nul,nul}, vacio(), []},
                 io:format("nuevo juego: ~p\n", [NewGame]),
                 PidGames!{self(),[NewGame | Games], all},
                 receive ok -> {new, ok, Id} end
    end.


replace(Id, J, []) -> [];
replace(Id, J, [{Id,_,_,_,_}|Xs]) -> [J|Xs];
replace(Id, J, [X|Xs]) -> [X|replace(Id, J, Xs)].

aceptarJuego(Socket, PidUp, Username, JuegoId, PidGames) ->
    PidGames!{self(), games, all},
    receive
        Games -> case lists:keyfind(JuegoId, 1, Games) of
                    false -> PidGames!{self(), Games, all},
                             receive ok -> {acc, error, "El juego no existe\n"} end;
                    {Id,{U1,P1},{U2,P2},T,O} -> if U2/=nul -> PidGames!{self(), Games, all},
                                                              receive ok -> {acc, error, "El juego no esta disponible para jugar\n"} end;
                                                   U1 == Username -> PidGames!{self(), Games, all},
                                                                     receive ok -> {acc, error, "No puede jugar contra si mismo\n"} end;
                                                   true -> GameAct = {Id, {U1,P1}, {Username, PidUp}, T, O},
                                                           PidGames!{self(), replace(Id, GameAct, Games), all},
                                                           receive ok -> movesRegister!{self(), move},
                                                                         receive Cmdid -> P1!{upd, Cmdid, "Tu partida ha sido aceptada por " ++ Username ++ "! Puedes comenzar a jugar.\n"},
                                                                                          movesRegister!{self(),ok}
                                                                         end,
                                                                         {acc, ok, Id}
                                                           end
                                                end
                 end
    end.

añadirObs(Socket, PidUp, Username, JuegoId, PidGames) ->
    PidGames!{self(), games, all},
    receive
        Games -> case lists:keyfind(JuegoId, 1, Games) of
                    false -> {obs, error, "El juego no existe\n"};
                    {Id,J1,J2,T,O} -> B = lists:member(Socket, O),
                                      if B -> GameAct = {Id, J1, J2, T, O};
                                         true -> GameAct = {Id, J1, J2, T, [PidUp |O]}
                                      end,
                                      PidGames!{self(), replace(Id, GameAct, Games), all},
                                      receive ok -> {obs, ok, Id} end
                end
    end.


realizarJugada(Socket, JuegoId, Username, X, Y, PidGames, PidMoves) ->
    PidGames!{self(), games, all},
    receive
        Games -> case lists:keyfind(JuegoId, 1, Games) of
                    false -> {pla, error, "El juego no existe\n"};
                    {Id,{U1,P1},{U2,P2},T,O} -> if (Username/=U1) and (Username/= U2) -> PidGames!{self(), Games, all},
                                                                                         receive ok -> {pla, error, "Usted no puede realizar jugadas en este juego\n"} end;
                                                   true -> NX = countLetter(T,"X"),
                                                           NO = countLetter(T,"O"),
                                                           if (Username == U1) and (NX>NO) -> PidGames!{self(), Games, all},
                                                                                              receive ok -> {pla, error, "No es su turno\n"} end;
                                                              (Username == U2) and (NX==NO) ->  PidGames!{self(), Games, all},
                                                                                                receive ok -> {pla, error, "No es su turno\n"} end;
                                                              true -> if Username == U1 -> C = "X",
                                                                                           J = U1,
                                                                                           R = P2;
                                                                         true -> C = "O",
                                                                                 J = U2,
                                                                                 R = P1
                                                                      end,
                                                                      NewT = pos(X,Y,T,C),
                                                                      if NewT == T -> PidGames!{self(), Games, all},
                                                                                      receive ok -> {pla, error, "Jugada erronea\n"} end;
                                                                         true -> Gano = gano(NewT,C),
                                                                                 Lleno = lleno(NewT),
                                                                                 if Gano -> Str1 = "\nEl jugador " ++ J ++ " es el ganador.\nFin del juego.\n",
                                                                                            Str2 = "Ganaste!!!\nFin del juego.\n";
                                                                                    Lleno -> Str1 = "\nEl juego ha terminado.\nEmpate.\n",
                                                                                             Str2 = Str1;
                                                                                    true -> Str1 = "\n",
                                                                                            Str2 = Str1
                                                                                 end,
                                                                                 PidMoves!{self(), move},
                                                                                 PrettyBoard = prettifyboard(NewT),
                                                                                 receive Cmdid -> lists:foreach(fun(Pid)-> Pid!{upd, Cmdid, Id ++ "\n" ++ PrettyBoard ++ Str1} end, [R|O]),
                                                                                                  PidMoves!{self(),ok}
                                                                                 end,
                                                                                 if Gano or Lleno -> ListAct = lists:delete({Id,{U1,P1},{U2,P2},T,O}, Games);
                                                                                    true -> ListAct = replace(Id, {Id,{U1,P1},{U2,P2},NewT,O}, Games)
                                                                                 end,
                                                                                 PidGames!{self(), ListAct, all},
                                                                                 receive ok -> {pla, ok, PrettyBoard, Str2} end
                                                                      end
                                                           end
                                                end
                 end
    end.

salirDelJuego(Socket, JuegoId, Username, PidGames, PidMoves) ->
    PidGames!{self(), games, all},
    receive
        Games -> case lists:keyfind(JuegoId, 1, Games) of
                    false -> {out, error, "El juego no existe\n"};
                    {Id,{U1,P1},{U2,P2},T,O} -> if (Username/=U1) and (Username/= U2) -> PidGames!{self(), Games, all},
                                                                                         receive ok -> {out, error, "Usted no es parte de este juego\n"} end;
                                                   true ->  if Username == U1 -> R = P2;
                                                               true -> R = P1
                                                            end,
                                                            PidMoves!{self(), move},
                                                            receive Cmdid -> lists:foreach(fun(Pid)-> Pid!{upd, Cmdid, Id ++ "\nEl jugador " ++ Username ++ " ha abandonado el juego.\n"} end, [R|O]),
                                                                             PidMoves!{self(),ok}
                                                            end,
                                                            ListAct = lists:delete({Id,{U1,P1},{U2,P2},T,O}, Games),
                                                            PidGames!{self(), ListAct, all},
                                                            receive ok -> {out, ok} end
                                                end
                 end
    end.

eliminarObs(Socket, JuegoId, PidGames) ->
    PidGames!{self(), games, all},
    receive
        Games -> case lists:keyfind(JuegoId, 1, Games) of
                    false -> {lea, error, "El juego no existe\n"};
                    {Id,J1,J2,T,O} -> B = lists:member(Socket, O),
                                      if B -> NewObs = lists:delete(Socket, O),
                                              GameAct = {Id, J1, J2, T, NewObs},
                                              NewGames = replace(Id, GameAct, Games),
                                              PidGames!{self(), NewGames, all},
                                              receive ok -> {lea, ok} end;
                                        true -> PidGames!{self(), Games, all},
                                                receive ok -> {lea, error, "Usted no es observador de este juego.\n"} end
                                      end
                 end
    end.

out_if_in(Socket, Username, {Id,{U1,P1},{U2,P2},T,O}, PidMoves) ->
    if (Username/=U1) and (Username/= U2) -> NewObs = lists:delete(Socket, O),
                                             {Id,{U1,P1},{U2,P2},T,NewObs};
       true ->  if Username == U1 -> R = P2;
                   true -> R = P1
                end,
                PidMoves!{self(), move},
                receive Cmdid -> lists:foreach(fun(Pid)-> Pid!{upd, Cmdid, Id ++ "\nEl jugador " ++ Username ++ " ha abandonado el juego.\n"} end, [R|O]),
                                 PidMoves!{self(),ok}
                end,
                nul
    end.

desconectar(Socket, Username, PidUsers, PidGames, PidMoves) ->
    PidGames!{self(), games, all},
    receive
        Games -> NewList = lists:map(fun(Game) -> out_if_in(Socket, Username, Game, PidMoves) end, Games),
                 ListAct = lists:filter(fun(Game) -> Game/=nul end, NewList),
                 PidGames!{self(), ListAct, all},
                 receive ok -> PidUsers!{self(), users, all},
                               receive Users -> NewUsers = lists:delete(Username, Users),
                                                PidUsers!{self(), NewUsers, all},
                                                receive ok -> {bye, ok} end
                               end
                 end
    end.
