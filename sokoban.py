from pyswip import Prolog
from gym_sokoban.envs.sokoban_env_fast import SokobanEnvFast


if __name__ == "__main__":
    dim_room = (8, 8)
    num_boxes = 2
    env = SokobanEnvFast(dim_room=dim_room, num_boxes=num_boxes)
    # The encoding of the board is described in README
    board = env.reset()

    wall = board[:,:,0] # this is a one-hot encoding of walls
    # For readibility first we deal with tops and then with rights
    print("Walls {}".format(wall))
    print("Walls shape {}".format(wall.shape))
    tops = []
    for i in range(dim_room[0]):
        for j in range(dim_room[1]-1):
            if wall[i,j] == 0 and wall[i,j+1] == 0:
                tops.append("top(x{}y{},x{}y{})".format(i,j,i,j+1))

    rights = []
    for i in range(dim_room[0]-1):
        for j in range(dim_room[1]):
            if wall[i,j] == 0 and wall[i,j+1] == 0:
                rights.append("right(x{}y{},x{}y{})".format(i,j,i+1,j))

    boxes_initial_locations = board[:,:,4]
    print("boxes_initial_locations {}".format(boxes_initial_locations))
    print("boxes_initial_locations shape {}".format(boxes_initial_locations.shape))
    boxes_initial = []
    for i in range(dim_room[0]):
        for j in range(dim_room[1]):
            if boxes_initial_locations[i,j] == 1:
                boxes_initial.append("box(x{}y{})".format(i,j))

    boxes_target_locations = board[:,:,2]
    print("boxes_target_locations {}".format(boxes_target_locations))
    print("boxes_target_locations shape {}".format(boxes_target_locations.shape))
    boxes_target = []
    for i in range(dim_room[0]):
        for j in range(dim_room[1]):
            if boxes_target_locations[i,j] == 1:
                boxes_target.append("solution(x{}y{})".format(i,j))


    sokoban_initial_location = board[:,:,5] + board[:,:,6]
    print("sokoban_initial_location {}".format(sokoban_initial_location))
    print("sokoban_initial_location shape {}".format(sokoban_initial_location.shape))
    for i in range(dim_room[0]):
        for j in range(dim_room[1]):
            if sokoban_initial_location[i,j] == 1:
                sokoban_string = "sokoban(x{}y{})".format(i,j)
                break

    tops_string = "[" + ','.join(tops) + ']'
    rights_string = "[" + ','.join(rights) + ']'
    boxes_initial_string = "[" + ','.join(boxes_initial) + ']'
    boxes_target_string = "[" + ','.join(boxes_target) + ']'

    print("Tops {}".format(tops_string))
    print("Rights {}".format(rights_string))
    print("Boxes initial locations {}".format(boxes_initial_string))
    print("Boxes target locations {}".format(boxes_target_string))
    print("Sokoban initial location {}".format(sokoban_string))

    # tops = "[top(x1y1,x1y2),top(x1y2,x1y3),top(x2y1,x2y2),top(x2y2,x2y3),top(x3y1,x3y2),top(x3y2,x3y3)]"
    # rights = "[right(x1y3, x2y3),right(x2y3, x3y3),right(x1y2, x2y2),right(x2y2, x3y2),right(x1y1, x2y1),right(x2y1, x3y1)]"
    # boxes = "[box(x2y2),box(x3y2)]"
    # solutions = "[solution(x2y1),solution(x3y1)]"
    # sokoban = "sokoban(x1y1)"


    prolog = Prolog()
    prolog.consult("sokoban.pl")
    query = "solve([{},{},{},{},{}],Solution)".format(tops_string,
                                                      rights_string,
                                                      boxes_initial_string,
                                                      boxes_target_string,
                                                      sokoban_string)

    print(query)
    result=list(prolog.query(query))
    print("Number of solutions:", len(result))

    for i, r in enumerate(result):
        solution = r['Solution']
        print("Solution {}:\n{}".format(i, solution))
