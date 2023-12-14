import sys

n = int(sys.argv[1])

with open("sexpr_sample.sexpr", "r", encoding="utf-8") as f:
    content = f.read()

with open(sys.argv[2], "w", encoding="utf-8") as f:
    for i in range(n):
        f.write(content)

print(f"Done! Replicated sample {n} times into {sys.argv[2]}")
