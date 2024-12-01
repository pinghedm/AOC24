from collections import defaultdict

left = []
right = []

with open("input.text") as f:
    for line in f:
        l, r = line.split()
        left.append(int(l))
        right.append(int(r))

dist = sum(abs(l - r) for (l, r) in zip(sorted(left), sorted(right)))

freqOfLeftInRight = {}
for l in left:
    times_in_right = len([x for x in right if x == l])
    freqOfLeftInRight[l] = times_in_right

similarity = sum(k * v for k, v in freqOfLeftInRight.items())

print("dist: ", dist)
print("sim: ", similarity)
