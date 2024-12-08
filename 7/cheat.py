import functools
import itertools
import operator
from concurrent.futures import ProcessPoolExecutor


def _perm_helper(acc, combo):
    val, op = combo
    return op(val, acc)


def run_perm(test, vals, ops):
    combo = list(zip(vals[1:], itertools.cycle(ops)))
    final_val = functools.reduce(_perm_helper, combo, vals[0])
    return final_val == test


def concat(arg1, arg2):
    return int(
        str(arg2) + str(arg1)
    )  # backwards cause reduce messed me up with the concat guy


@functools.cache
def get_perms(num_operators):
    base = [[operator.add, operator.mul, concat]] * num_operators
    perms = list(itertools.product(*base))
    return perms


print("reading file")
with open("input.text") as f:
    parsed_lines = []
    for line in f:
        test, vals = line.split(": ")
        parsed_lines.append((int(test), [int(x) for x in vals.strip().split(" ")]))
print("done reading file")


jobs = []
total = 0
for idx, (test, vals) in enumerate(parsed_lines):
    num_operators = len(vals) - 1
    perms = get_perms(num_operators)
    for perm in perms:
        checks_out = run_perm(test, vals, perm)
        if checks_out:
            total += test
            break

print("valid lines", total)
