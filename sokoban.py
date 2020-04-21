from pyswip import Prolog
from gym_sokoban.envs.sokoban_env_fast import SokobanEnvFast
import time
import random

# example_query = "call_with_time_limit(5,solve([[top(x1y1,x1y2),top(x1y2,x1y3),top(x1y3,x1y4),top(x1y4,x1y5),top(x1y5,x1y6),top(x2y1,x2y2),top(x2y2,x2y3),top(x2y3,x2y4),top(x2y4,x2y5),top(x2y5,x2y6)],[right(x1y1,x2y1),right(x1y2,x2y2),right(x1y3,x2y3),right(x1y4,x2y4),right(x1y5,x2y5),right(x1y6,x2y6),right(x2y6,x3y6)],[box(x1y4),box(x2y6)],[solution(x1y2),solution(x1y6)],sokoban(x3y6)],Solution))"

def flatten(container):
    for i in container:
        if isinstance(i, (list,tuple)):
            for j in flatten(i):
                yield j
        else:
            yield i

def find_solution(size=8, num_boxes=2, time_limit=10):
    dim_room = (size, size)

    env = SokobanEnvFast(dim_room=dim_room, num_boxes=num_boxes, seed=0)
    # The encoding of the board is described in README
    board = env.reset()

    wall = board[:,:,0] # this is a one-hot encoding of walls
    # For readibility first we deal with tops and then with rights
    # print("Walls {}".format(wall))
    # print("Walls shape {}".format(wall.shape))
    tops = []
    for i in range(dim_room[0]):
        for j in range(dim_room[1]-1):
            if wall[i,j] == 0 and wall[i,j+1] == 0:
                tops.append("top(x{}y{},x{}y{})".format(i,j,i,j+1))

    rights = []
    for i in range(dim_room[0]-1):
        for j in range(dim_room[1]):
            if wall[i,j] == 0 and wall[i+1,j] == 0:
                rights.append("right(x{}y{},x{}y{})".format(i,j,i+1,j))

    boxes_initial_locations = board[:,:,4]
    # print("boxes_initial_locations {}".format(boxes_initial_locations))
    # print("boxes_initial_locations shape {}".format(boxes_initial_locations.shape))
    boxes_initial = []
    for i in range(dim_room[0]):
        for j in range(dim_room[1]):
            if boxes_initial_locations[i,j] == 1:
                boxes_initial.append("box(x{}y{})".format(i,j))

    boxes_target_locations = board[:,:,2]
    # print("boxes_target_locations {}".format(boxes_target_locations))
    # print("boxes_target_locations shape {}".format(boxes_target_locations.shape))
    boxes_target = []
    for i in range(dim_room[0]):
        for j in range(dim_room[1]):
            if boxes_target_locations[i,j] == 1:
                boxes_target.append("solution(x{}y{})".format(i,j))

    sokoban_initial_location = board[:,:,5] + board[:,:,6]
    # print("sokoban_initial_location {}".format(sokoban_initial_location))
    # print("sokoban_initial_location shape {}".format(sokoban_initial_location.shape))
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

    prolog = Prolog()
    prolog.consult("sokoban.pl")
    query = "call_with_time_limit({},solve([{},{},{},{},{}],Solution))".format(time_limit,
                                                                               tops_string,
                                                                               rights_string,
                                                                               boxes_initial_string,
                                                                               boxes_target_string,
                                                                               sokoban_string)

    # query=example_query
    print(query)
    # try:
    result = list(prolog.query(query))
    rewards = []

    print(board[:,:,0] +
      2*board[:,:,2] +
      3*board[:,:,3] +
      4*board[:,:,4] +
      5*board[:,:,5] +
      6*board[:,:,6])
    for i, r in enumerate(result):
        solution = r['Solution']
        actions = []
        print("Solution {}: {}".format(i, solution))
        for index in range(len(solution)):
            print("Solution at index {}".format(solution[index]))
            move = str(solution[index]).split()[-1]
            print("Move {}".format(move))
            move = move[:-1]
            action = map_moves(move)
            print("Move {} Action {}".format(move, action))
            actions.append(action)
            obs = env.step(action)
            board = obs[0]
            print(board[:,:,0] +
                  2*board[:,:,2] +
                  3*board[:,:,3] +
                  4*board[:,:,4] +
                  5*board[:,:,5] +
                  6*board[:,:,6])
            rewards.append(obs[1])
        print("Actions: {}".format(actions))
        print("Rewards: {}".format(rewards))
    #     return result
    # except:
    #     return None

def map_moves(move):
    if move == "up":
        return 3
    elif move == "down":
        return 2
    elif move == "left":
        return 0
    elif move == "right":
        return 1


if __name__ == "__main__":

    random.seed(0)
    number_of_trials = 1
    time_start = time.time()

    results = []
    for i in range(number_of_trials):
        result = find_solution(size=8, num_boxes=2, time_limit=20)
        if result is not None:
            results.append(result)


    print("Number of solutions: {}".format(len(results)))
    print("Total time: {}".format(time.time() - time_start))
