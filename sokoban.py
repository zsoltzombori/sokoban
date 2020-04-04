from pyswip import Prolog

prolog = Prolog()

prolog.consult("sokoban.pl")


tops = "[top(x1y1,x1y2),top(x1y2,x1y3),top(x2y1,x2y2),top(x2y2,x2y3),top(x3y1,x3y2),top(x3y2,x3y3)]"
rights = "[right(x1y3, x2y3),right(x2y3, x3y3),right(x1y2, x2y2),right(x2y2, x3y2),right(x1y1, x2y1),right(x2y1, x3y1)]"
boxes = "[box(x2y2),box(x3y2)]"
solutions = "[solution(x2y1),solution(x3y1)]"
sokoban = "sokoban(x1y1)"


query = "solve([{},{},{},{},{}],Solution)".format(tops, rights, boxes, solutions, sokoban)
result=list(prolog.query(query))

for i, r in enumerate(result):
    solution = r['Solution']
    print("Solution {}:\n{}".format(i, solution))
