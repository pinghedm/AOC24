with open("input.text") as f:
    lines = [line for line in f]

coords_of_x = set()
coords_of_m = set()
coords_of_a = set()
coords_of_s = set()

for i, line in enumerate(lines):
    for j, char in enumerate(line):
        match char:
            case "X":
                coords_of_x.add((i, j))
            case "M":
                coords_of_m.add((i, j))
            case "A":
                coords_of_a.add((i, j))
            case "S":
                coords_of_s.add((i, j))

col_len = len(lines[0])
row_len = len(lines)


def one_space_away(i, j):
    return {
        (i - 1, j),
        (i + 1, j),
        (i, j - 1),
        (i, j + 1),
        (i - 1, j - 1),
        (i + 1, j - 1),
        (i - 1, j + 1),
        (i + 1, j + 1),
    }


def get_direction(i1, j1, i2, j2):
    diffX = i1 - i2
    diffY = j1 - j2
    return (diffX, diffY)


xmas_count = 0
for xi, xj in coords_of_x:
    matching_ms = one_space_away(xi, xj) & coords_of_m
    for mi, mj in matching_ms:
        dx, dy = get_direction(xi, xj, mi, mj)
        a_pos = mi - dx, mj - dy
        s_pos = mi - dx - dx, mj - dy - dy
        if (a_pos in coords_of_a) and (s_pos in coords_of_s):
            xmas_count += 1
print("xmas count: ", xmas_count)


def one_diagonal_away(i, j):
    return {
        (i - 1, j - 1),
        (i + 1, j - 1),
        (i - 1, j + 1),
        (i + 1, j + 1),
    }


x_mas_count = 0
for mi, mj in coords_of_m:
    matching_as = coords_of_a & one_diagonal_away(mi, mj)
    for ai, aj in matching_as:
        # if there is an A diagonal to an M then to qualify there must be
        # an s in the direction of travel
        # an m and an s on the other diagonal
        dx, dy = get_direction(mi, mj, ai, aj)
        required_s = ai - dx, aj - dy  # s on this diagonal
        other_diagonal_one = ai + dx, aj - dy
        other_diagonal_two = ai - dx, aj + dy
        if (required_s in coords_of_s) and (
            (other_diagonal_one in coords_of_m and other_diagonal_two in coords_of_s)
            or (other_diagonal_one in coords_of_s and other_diagonal_two in coords_of_m)
        ):
            x_mas_count += 1
print(
    "x-mas count: ", x_mas_count / 2
)  # this really counts every diagonal run that is part of another diagonal run rather than x's, so divide by 2
