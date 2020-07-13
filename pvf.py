import random
import numpy as np
import time

from gym_sokoban.envs.sokoban_env_fast import SokobanEnvFast

class DetEnv(SokobanEnvFast):

    def __init__(self, dim_room, num_boxes, seed):
        self.init_state = None
        self.init_board = None
        
        super().__init__(dim_room=dim_room, num_boxes=num_boxes, seed=seed)

    def reset(self):
        if self.init_state is not None:
            self.restore_full_state(self.init_state)
        else:
            self.init_board = super().reset()
            self.init_state = self.clone_full_state()
        return self.init_board

    def successor_board(self, action):
        current_state = self.clone_full_state()
        new_board, _, _, _ = env.step(action)
        env.restore_full_state(current_state)
        return new_board
    
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
                
def vis_steps(env):
    for step in (0,1,2,3):
        board = env.reset()
        new_board, _, _, _ = env.step(step)
        print("\nStep ", step),
        display_board(board)
        display_board(new_board)

def features(board):
    box_target = board[:,:,3]
    box = board[:,:,3] + board[:,:,4]
    player = board[:,:,5] + board[:,:,6]
    board = np.stack([box_target, box, player], axis=-1)
    return board.flatten()
        
def feature_diff(board, new_board):
    board = features(board)
    new_board = features(new_board)
    return new_board - board

def add_entry(transitions, hashes, entry):
    hashable_entry = tuple(entry)
    if hashable_entry not in hashes:
        hashes.add(hashable_entry)
        transitions.append(entry)

def add_rollout(transitions, hashes, env, max_steps=10):
    board = env.reset()
    episode_over=False
    while (not episode_over) and max_steps > 0:
        action = random.randint(0,3)
        new_board, _reward, episode_over, info = env.step(action)
        max_steps -= 1
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
    print("Incidence matrix time : ", T1-T0, " sec")
    return matrix

def greedy_eval_move(env, pvf, max_steps=10):
    board = env.reset()
    display_board(board)
    for step in range(max_steps):
        current_value = np.dot(features(board), pvf)
        succ_values = []
        for action in range(4):
            succ_board = env.successor_board(action)
            succ_values.append(np.dot(features(succ_board), pvf))
        best_move = np.argmax(succ_values)
        if succ_values[best_move] <= current_value:
            print("STOPPING: Reached local maximum")
            break
        else:
            print("Current: {}, successors: {}, selecting {}".format(current_value, succ_values, best_move))
            board, _, _, _ = env.step(best_move)
            display_board(board)



env = DetEnv(dim_room=(6,6), num_boxes=2, seed=100)
incidence_matrix = create_incidence_matrix(env, 10000, 30)

std = np.std(incidence_matrix, axis=0)
mask = std > 0

real_incidence_matrix = incidence_matrix[:,mask]
print("After removing dummy features: ", real_incidence_matrix.shape)

u, s, vh = np.linalg.svd(real_incidence_matrix)

pvfs = np.zeros((len(mask), vh.shape[1]))
pvfs[mask] = vh

print("Evaluating greedy adherence to pvfs:")
for i in [0, 1, 2, 3, 4]:
    print("\n\nEvaluating pvf with {}th largest eigenvalue ({})".format(i, s[i]))
    greedy_eval_move(env, pvfs[:,i], max_steps=5)

for i in [1, 2, 3, 4, 5]:
    print("\n\nEvaluating pvf with {}th smallest eigenvalue ({})".format(i, s[-i]))
    greedy_eval_move(env, pvfs[:,-i], max_steps=5)
