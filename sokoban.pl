% taken from: https://github.com/jgsogo/sokoban
               
% sokoban\sokoban.pl: depth-first search implementation (applied to Sokoban puzzle)
%     ... learning Prolog with SWI-Prolog
% Author: Javier G. Sogo

/***************************************************************************/
/* Depth-first search solving framework                                    */
/***************************************************************************/
/* Depth-first search (DFS) is an algorithm for traversing or searching    */
/* tree or graph data structures. This algorithm can be applied to         */
/* combinatorial problems and guarantees to find the optimal solution      */
/* as it traverses the entire graph. Nevertheless for large graphs time    */
/* and/or resources may be a limitation.                                   */
/*                                                                         */
/***************************************************************************/

/* The problem is solved if the state is equal to final_state.             */
solve_dfs(State, _History, []) :-
    final_state(State), !.

/* If not, we have to explore new states                                   */
solve_dfs(State, History, Moves):-
    movement(State, BoxMove, SokobanMoves),
    append(SokobanMoves,[BoxMove], CurrentMoves),
    append(CurrentMoves, Moves2, Moves),
    update(State, BoxMove, NewState),
    \+ member(NewState, History),   /* No quiero ciclos en el grafo de búsqueda */
    solve_dfs(NewState, [NewState|History], Moves2).

/* Actually solve the problem                                              */
solve_problem(State, Solution) :-
    format('Initial state: ~w~n', State),
    solve_dfs(State, [State], Solution).


/***************************************************************************/
/* Application to Sokoban: include game rules and board layout             */
/***************************************************************************/

/* Game rules                                                              */
:-include(game).

/* Modify level file include to play other boards                          */
%% :-include(level1).

:- dynamic top/2.
:- dynamic right/2.
:- dynamic solution/1.
:- dynamic initial_state/2.

problem([[top(x1y1,x1y2),
          top(x1y2,x1y3),
          top(x2y1,x2y2),
          top(x2y2,x2y3),
          top(x3y1,x3y2),
          top(x3y2,x3y3)],
         [right(x1y3, x2y3),
          right(x2y3, x3y3),
          right(x1y2, x2y2),
          right(x2y2, x3y2),
          right(x1y1, x2y1),
          right(x2y1, x3y1)],
         [box(x2y2),
          box(x3y2)],
         [solution(x2y1),
          solution(x3y1)],
         sokoban(x1y1)]).

problem2([[top(x2y3,x2y4),top(x3y1,x3y2),top(x3y2,x3y3),top(x3y3,x3y4),top(x4y1,x4y2),top(x4y2,x4y3),top(x4y3,x4y4)],[right(x2y3,x3y3),right(x2y4,x3y4),right(x3y1,x4y1),right(x3y2,x4y2),right(x3y3,x4y3),right(x3y4,x4y4)],[box(x3y3),box(x4y3)],[solution(x2y3),solution(x4y1)],sokoban(x3y4)]).


init(Problem, State):-
    Problem = [Tops, Rights, Boxes, Solutions, sokoban(Sokoban)],
    init_board(Tops, Rights, Solutions),
    findall(Box, member(box(Box), Boxes), BoxLocs),
    State = state(Sokoban, BoxLocs),
    display_board(9, State).

init_board(Tops, Rights, Solutions):-
    abolish_all_tables,
    retractall(top(_,_)),
    findall(_, ( member(P, Tops), assert(P) ), _),
    retractall(right(_,_)),
    findall(_, ( member(P, Rights), assert(P) ), _),
    retractall(solution(_)),
    findall(_, ( member(P, Solutions), assert(P) ), _).
    

solve(Problem, Solution):-
    init(Problem, State),
    solve_problem(State, Solution).

step(sokoban(Dir), state(Sokoban, Boxes), state(NextLoc, Boxes), Steps):-
    neib(Sokoban,NextLoc,Dir),
    can_reach(Sokoban,NextLoc, Boxes, [], Steps), !.
step(push(BoxLoc,Dir), State, NewState, Steps):-
    movement(State, push(BoxLoc,Dir), SokobanMoves),
    append(SokobanMoves, [push(BoxLoc,Dir)], Steps),
    update(State, push(BoxLoc,Dir), NewState), !.
step(pushSequence(BoxLoc,TargetLoc), State, NewState, Steps):-
    pushSequence(State, BoxLoc, TargetLoc, [], NewState, Steps), !.
% step(moveBox(BoxLoc,TargetLoc), State, NewState, Steps):-
    

    
display_board(Size, State):-
    write("  "), foreach(between(1,Size,X), format("~w ", [X])), writeln(""),
    write("  "), foreach(between(1,Size,_), write("_ ")), writeln(""),
    foreach(between(1,Size,Y0), display_line(Size, Y0, State)).

display_line(Size,Y0,State):-
    Y is Size-Y0 + 1,
    format("~w|", [Y]),
    foreach(between(1,Size,X), (
                display_cell(X,Y,State)
            )),
    writeln("").


display_cell(X,Y,state(Sokoban, Boxes)):-
    format(atom(Loc), "x~wy~w", [X,Y]),
    ( Loc = Sokoban -> write("S")
    ; member(Loc, Boxes) -> write("B")
    ; solution(Loc) -> write("X")
    ; top(_, Loc) -> write(" ")
    ; write("_")
    ),
    ( right(Loc,_) -> write(" ") ; write("|") ).    
    
    

/***************************************************************************/
/* Utility rules                                                           */
/***************************************************************************/
clear_screen :-
    format('~c~s', [0x1b, "[2J"]).  /* Credit: http://stackoverflow.com/questions/16908764/clearing-screen-in-swipl-prolog-in-windows */


%%%%%%%%%%%%%%%%%%%%%%
% Unit tests
%%%%%%%%%%%%%%%%%%%%%%
:- begin_tests(sokoban).
:- end_tests(sokoban).

%:- run_tests.
