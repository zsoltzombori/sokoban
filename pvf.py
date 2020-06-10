import random
import numpy as np
import time

from gym_sokoban.envs.sokoban_env_fast import SokobanEnvFast

def map_moves(move):
    if move == "up":
        return 3
    elif move == "down":
        return 2
    elif move == "left":
        return 0
    elif move == "right":
        return 1

def display_board(board):
    print()
    for y in range(board.shape[1]-1, -1, -1):
        print()
        for x in range(board.shape[0]):
            cell = board[x,y]
            if cell[0] == 1:
                print("*", end="")
            elif cell[1] == 1:
                print(" ", end="")
            elif cell[2] == 1:
                print("T", end="")
            elif cell[3] == 1:
                print("B", end="")
            elif cell[4] == 1:
                print("b", end="")
            elif cell[5] == 1:
                print("s", end="")
            elif cell[6] == 1:
                print("S", end="")
                
    # print(board[:,:,0] +
    #       2*board[:,:,2] +
    #       3*board[:,:,3] +
    #       4*board[:,:,4] +
    #       5*board[:,:,5] +
    #       6*board[:,:,6])

def vis_steps(env):
    for step in (0,1,2,3):
        board = env.reset()
        new_board, _, _, _ = env.step(step)
        print("\nStep ", step),
        display_board(board)
        display_board(new_board)

def feature_diff(board, new_board):
    diff = new_board - board
    return diff.flatten()

def add_entry(transitions, hashes, entry):
    hashable_entry = tuple(entry)
    if hashable_entry not in hashes:
        hashes.add(hashable_entry)
        transitions.append(entry)

def add_rollout(transitions, hashes, env, max_steps=10):
    board = env.reset()
    # print("\n\n Rollout")
    # display_board(board)
    episode_over=False
    while (not episode_over) and max_steps > 0:
        action = random.randint(0,3)
        new_board, _reward, episode_over, info = env.step(action)
        max_steps -= 1
        # print("Action: ", action)
        # print(episode_over, info)
        # display_board(new_board)
        entry = feature_diff(board, new_board)
        board = new_board
        add_entry(transitions, hashes, entry)

def create_incidence_matrix(env, rollouts, max_steps):
    T0 = time.time()
    transitions = []
    hashes = set()
    for _ in range(rollouts):
        add_rollout(transitions, hashes, env, max_steps=max_steps)
    matrix = np.array(transitions)
    T1 = time.time()
    print("Incidence matrix shape: ", matrix.shape)
    print("Time: ", T1-T0, " sec")
    return matrix

def greedy_eval_move(env, pvf, max_steps=10, state = None, board = None):
    if state is None or board is None:
        board = env.reset()
        state = env.clone_full_state()
    else:
        env.restore_full_state(state)
    display_board(board)
    for step in range(max_steps):
        current_value = np.dot(board.flatten(), pvf)
        succ_values = []
        for action in range(4):
            succ_board, _, _, _ = env.step(action)
            succ_values.append(np.dot(succ_board.flatten(), pvf))
            env.restore_full_state(state)
        best_move = np.argmax(succ_values)
        if succ_values[best_move] == current_value:
            print("STOPPING: Reached local maximum")
            break
        else:
            print("Current: {}, successors: {}, selecting {}".format(current_value, succ_values, best_move))
            board, _, _, _ = env.step(best_move)
            state = env.clone_full_state()
            display_board(board)



env = SokobanEnvFast(dim_room=(8,8), num_boxes=2, seed=0)
# vis_steps(env)
incidence_matrix = create_incidence_matrix(env, 10000, 100)

u, s, vh = np.linalg.svd(incidence_matrix)

board = env.reset()
state = env.clone_full_state()
print("Evaluating greedy adherence to pvfs:")
for i in [0, 1, 2, 3, 4]:
    print("\n\nEvaluating pvf with {}th largest eigenvalue ({})".format(i, s[i]))
    greedy_eval_move(env, vh[:,i], max_steps=5, state=state, board=board)

for i in [1, 2, 3, 4, 5]:
    print("\n\nEvaluating pvf with {}th smallest eigenvalue ({})".format(i, s[-i]))
    greedy_eval_move(env, vh[:,-i], max_steps=5, state=state, board=board)
