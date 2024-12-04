import re

mul_regex = re.compile(r"mul\((\d{1,3}),(\d{1,3})\)")
with open("input.text") as f:
    instructions = f.read()


matches = re.findall(mul_regex, instructions)
ans = sum(int(a) * int(b) for (a, b) in matches)
print("sum of muls:", ans)


valid_muls = []
mul_enabled = True
do_regex = re.compile(r"do\(\)")
dont_regex = re.compile(r"don't\(\)")
while len(instructions):
    matches = {
        "mul": re.search(mul_regex, instructions),
        "dont": re.search(dont_regex, instructions),
        "do": re.search(do_regex, instructions),
    }
    if not matches["mul"]:
        # if there are no more muls, then we're done anyway
        break
    sorted_matches = sorted(
        [(k, v) for k, v in matches.items() if v], key=lambda t: t[1].span()[0]
    )
    if not sorted_matches:
        break
    first_match_type, first_match = sorted_matches[0]
    match first_match_type:
        case "mul":
            if mul_enabled:
                valid_muls.append(first_match.groups())
        case "do":
            mul_enabled = True
        case "dont":
            mul_enabled = False
    instructions = instructions[first_match.span()[1] :]


ans = sum(int(a) * int(b) for (a, b) in valid_muls)
print("sum of enabled muls:", ans)
