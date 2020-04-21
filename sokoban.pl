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
solve_dfs(Problem, State, _History, []) :-
    final_state(Problem, State), !.

/* If not, we have to explore new states                                   */
solve_dfs(Problem, State, History, Moves):-
    movement(State, BoxMove, SokobanMoves),
    append([SokobanMoves,[BoxMove],Moves2], Moves),
    update(State, BoxMove, NewState),
    \+ member(NewState, History),   /* No quiero ciclos en el grafo de búsqueda */
    solve_dfs(Problem, NewState, [NewState|History], Moves2).

/* Actually solve the problem                                              */
solve_problem(Problem, Solution) :-
    format('=============~n'),
    format('|| Problem: ~w~n', Problem),
    format('=============~n'),
    initial_state(Problem, Initial),
    format('Initial state: ~w~n', Initial),
    solve_dfs(Problem, Initial, [Initial], Solution).


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

problem2([[top(x2y1,x2y2),top(x2y2,x2y3),top(x3y1,x3y2),top(x3y2,x3y3),top(x4y1,x4y2),top(x5y1,x5y2),top(x5y2,x5y3),top(x5y5,x5y6),top(x6y1,x6y2),top(x6y2,x6y3),top(x6y3,x6y4),top(x6y4,x6y5),top(x6y5,x6y6)],[right(x2y1,x3y1),right(x2y2,x3y2),right(x3y1,x4y1),right(x3y2,x4y2),right(x4y1,x5y1),right(x5y1,x6y1),right(x5y2,x6y2),right(x5y5,x6y5),right(x6y1,x7y1),right(x6y2,x7y2),right(x6y3,x7y3),right(x6y4,x7y4),right(x6y5,x7y5)],[box(x3y2),box(x5y2)],[solution(x2y3),solution(x6y6)],sokoban(x4y1)]).

solve(Problem, Solution):-
    Problem = [Tops, Rights, Boxes, Solutions, sokoban(Sokoban)],
    abolish_all_tables,
    retractall(top(_,_)),
    findall(_, ( member(P, Tops), assert(P) ), _),
    retractall(right(_,_)),
    findall(_, ( member(P, Rights), assert(P) ), _),
    retractall(solution(_)),
    findall(_, ( member(P, Solutions), assert(P) ), _),
    
    retractall(initial_state(_,_)),
    findall(Box, member(box(Box), Boxes), BoxLocs),
    assert(initial_state(sokoban, state(Sokoban, BoxLocs))),
    display_board(9),
    solve_problem(sokoban, Solution).
%% convert_solution(Solution0, Solution).

convert_solution([],[]).
convert_solution([[move(Box,Dir),SokobanMoves]|As],[[[Box2,Dir2],SokobanMoves]|Bs]):-
    atom_string(Box, Box2),
    atom_string(Dir, Dir2),
    convert_solution(As, Bs).
    
display_board(Size):-
    write("  "), foreach(between(1,Size,X), format("~w ", [X])), writeln(""),
    write("  "), foreach(between(1,Size,_), write("_ ")), writeln(""),
    foreach(between(1,Size,Y0), display_line(Size, Y0)).

display_line(Size,Y0):-
    Y is Size-Y0 + 1,
    format("~w|", [Y]),
    foreach(between(1,Size,X), (
                display_cell(X,Y)
            )),
    writeln("").


display_cell(X,Y):-
    format(atom(Loc), "x~wy~w", [X,Y]),
    initial_state(_, state(Sokoban, Boxes)),
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
