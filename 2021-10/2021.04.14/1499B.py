t = int(input())

for _ in range(t):
    line = input()
    output = True
    for i in range(len(line) - 1):
        if line[i] == '1' and line[i + 1] == '1':
            for j in range(i + 2, len(line) - 1):
                if line[j] == '0' and line[j + 1] == '0':
                    output = False
                    break
    if output:
        print("YES")
    else:
        print("NO")


