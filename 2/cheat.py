def row_is_safe_dist(row):
    paired_numbers = zip(row, row[1:])
    dists = [abs(x - y) for x, y in paired_numbers]
    return all(1 <= d <= 3 for d in dists)


def row_is_sorted(row):
    return row == sorted(row) or row == sorted(row, reverse=True)


def row_is_safe(r):
    return row_is_safe_dist(r) and row_is_sorted(r)


def all_permutations_of_row(row):
    rows = [row]
    for idx in range(len(row)):
        row_copy = list(row)
        row_copy.pop(idx)
        rows.append(row_copy)
    return rows


reports = []

with open("input.text") as f:
    for line in f:
        vals = line.split()
        reports.append([int(x) for x in vals])

plain_safe_rows = [r for r in reports if row_is_safe(r)]
damped_safe_rows = []
for row in reports:
    version_of_row = all_permutations_of_row(row)
    if any(row_is_safe(r) for r in version_of_row):
        damped_safe_rows.append(row)

print("Undamped count: ", len(plain_safe_rows))
print("Damped count: ", len(damped_safe_rows))
