import enum

with open("input.text") as f:
    board = []
    for line in f:
        board.append(list(line.strip()))


class Facing(enum.StrEnum):
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


def spot_out_of_bounds(coords: tuple[int, int], board: list[list[str]]):
    x, y = coords
    if x < 0 or y < 0:
        return True
    if x > len(board[0]) - 1:
        return True
    if y > len(board) - 1:
        return True


def get_guard_pos_and_facing(board):
    guard_facing = None
    for y, row in enumerate(board):
        for x, col in enumerate(row):
            guard_facing = char_is_guard_and_facing(col)
            if guard_facing:
                return (x, y), guard_facing
    return (-1, -1), None


def update_board(board: list[list[str]], visited_cells: set[tuple[int, int]]):
    (guard_x, guard_y), guard_facing = get_guard_pos_and_facing(board)
    if guard_facing is None:
        raise Exception("Guard Not Found")
    visited_cells.add((guard_x, guard_y))

    infront_x, infront_y = get_coords_in_facing_dir((guard_x, guard_y), guard_facing)
    if spot_out_of_bounds((infront_x, infront_y), board):
        raise Exception("Guard Left Board!")
    print(guard_x, guard_y, guard_facing, infront_x, infront_y)
    infront_char = board[infront_y][infront_x]
    if infront_char == ".":
        board[guard_y][guard_x] = "."
        board[infront_y][infront_x] = char_by_facing[guard_facing]
    elif infront_char == "#":
        new_facing = rotation_result(guard_facing)
        board[guard_y][guard_x] = char_by_facing[new_facing]
    return board, visited_cells


visited_cells = set()
while True:
    try:
        board, visited_cells = update_board(board, visited_cells)
    except Exception as e:
        import traceback

        traceback.print_exc()
        break

print("Guard visited: ", len(visited_cells), "cell")
