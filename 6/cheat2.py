import copy
import enum
import multiprocessing
import random
import sys
import time
from collections import defaultdict
from concurrent.futures import ProcessPoolExecutor

with open("input.text") as f:
    board = {}
    for y, line in enumerate(f):
        for x, char in enumerate(line.strip()):
            board[(x, y)] = char


class Facing(enum.Enum):
    UP = enum.auto()
    RIGHT = enum.auto()
    DOWN = enum.auto()
    LEFT = enum.auto()


def rotation_result(initial_facing):
    if initial_facing == Facing.UP:
        return Facing.RIGHT
    elif initial_facing == Facing.RIGHT:
        return Facing.DOWN
    elif initial_facing == Facing.DOWN:
        return Facing.LEFT
    elif initial_facing == Facing.LEFT:
        return Facing.UP

    raise Exception("Illegal Guard Position")


facing_by_char = {"<": Facing.LEFT, ">": Facing.RIGHT, "^": Facing.UP, "v": Facing.DOWN}

char_by_facing = {v: k for k, v in facing_by_char.items()}


def char_is_guard_and_facing(char):
    return facing_by_char.get(char)


def get_coords_in_facing_dir(guard_pos, guard_facing):
    x, y = guard_pos
    if guard_facing == Facing.LEFT:
        return (x - 1, y)
    elif guard_facing == Facing.UP:
        return (x, y - 1)
    elif guard_facing == Facing.RIGHT:
        return (x + 1, y)
    elif guard_facing == Facing.DOWN:
        return (x, y + 1)
    return (-1, -1)


def spot_out_of_bounds(coords: tuple[int, int]):
    x, y = coords
    if x < 0 or y < 0:
        return True
    if x > 129 - 1:
        return True
    if y > 129 - 1:
        return True


def get_guard_pos_and_facing(
    board: dict[tuple[int, int], str]
) -> tuple[tuple[int, int], Facing | None]:
    for coords, char in board.items():
        if char in facing_by_char:
            return coords, facing_by_char[char]
    return ((-1, -1), None)


def update_board(
    board: dict[tuple[int, int], str], visited_cells: set[tuple[int, int, Facing]]
):
    (guard_x, guard_y), guard_facing = get_guard_pos_and_facing(board)
    if guard_facing is None:
        raise Exception("Guard Not Found")
    if (guard_x, guard_y, guard_facing) in visited_cells:
        return board, visited_cells, "loop found"
    visited_cells.add((guard_x, guard_y, guard_facing))
    infront_x, infront_y = get_coords_in_facing_dir((guard_x, guard_y), guard_facing)
    if spot_out_of_bounds((infront_x, infront_y)):
        return board, visited_cells, "guard escaped"
    infront_char = board[(infront_x, infront_y)]
    if infront_char == ".":
        board[(guard_x, guard_y)] = "."
        board[(infront_x, infront_y)] = char_by_facing[guard_facing]
    elif infront_char == "#":
        new_facing = rotation_result(guard_facing)
        board[(guard_x, guard_y)] = char_by_facing[new_facing]
    return board, visited_cells, ""


test = {(x, y) for x in range(100) for y in range(100)}

print("generating boards")
start = time.time()
modified_boards = []
for x, y in board.keys():
    char = board[(x, y)]
    if char in ["^", "#"]:
        continue
    modified_board = {**board}
    modified_board[(x, y)] = "#"
    modified_boards.append((x, y, modified_board))
print(f"done generating {len(modified_boards)} boards")
end = time.time()
print("took ", end - start)


def test_run(board_info):
    x, y, board = board_info
    print("starting board modified at ", x, y)
    visited_cells = set()
    while True:
        try:
            board, visited_cells, message = update_board(board, visited_cells)
            if message:
                print("finished board modified at ", x, y)
                if message == "loop found":
                    return 1
                return 0
        except Exception as e:
            print(e)
            return 0


test = list(range(1000))
with ProcessPoolExecutor() as executor:
    results = executor.map(test_run, modified_boards)
    print(sum(results))
