import random
from collections import defaultdict

with open("input.text") as f:
    rules = []
    update_runs = []
    reading_rules = True
    for _line in f:
        line = _line.removesuffix(r"\n")
        if len(line) == 1:
            reading_rules = False
            continue
        elif reading_rules:
            before, after = line.split("|")
            rules.append((int(before), int(after)))
        else:
            run = [int(x) for x in line.split(",")]
            update_runs.append(run)

must_be_before_by_page: dict[int, set[int]] = defaultdict(set)
for before, after in rules:
    must_be_before_by_page[after].add(before)

good_runs = []
bad_runs = []


def run_is_good(run, must_be_before_by_page):
    good = True
    for idx, page in enumerate(run):
        pages_that_must_be_before = must_be_before_by_page[page]
        pages_to_care_about = pages_that_must_be_before & set(run)
        if pages_to_care_about & set(run[idx:]):
            good = False
    return good


for run in update_runs:
    if run_is_good(run, must_be_before_by_page):
        good_runs.append(run)
    else:
        bad_runs.append(run)


sum_of_middles = sum(l[len(l) // 2] for l in good_runs)
print("sum: ", sum_of_middles)


def reorder(run, must_be_before_by_page):
    new_run = []
    unordered_pages = set(run)
    while len(unordered_pages):
        page = unordered_pages.pop()
        pages_that_must_be_before = must_be_before_by_page[page]
        if unordered_pages & pages_that_must_be_before:
            # TODO
            unordered_pages.add(page)
        else:
            # if there are no pages that must be before me left to order, then I am ok
            new_run.append(page)
    return new_run


fixed_runs = []
for run in bad_runs:
    fixed_run = reorder(run, must_be_before_by_page)
    if not run_is_good(fixed_run, must_be_before_by_page):
        print("still bad", run, fixed_run)
    else:
        fixed_runs.append(fixed_run)

sum_of_middles = sum(l[len(l) // 2] for l in fixed_runs)
print("sum: ", sum_of_middles)
