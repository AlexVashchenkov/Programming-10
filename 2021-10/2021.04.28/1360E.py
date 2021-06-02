n = int(input())

m = []

for i in range(n):
    size = int(input())
    elem = [[int(j) for j in input()] for k in range(size)]
    m += [elem]

def check_zero(l):
    size = len(l[0])
    for i in range(size):
        for j in range(size):
            if l[i][j] != 0:
                return False
    return True

def first_rule(l):
    size = len(l[0])
    for i in range(size):
        if l[i][size-1] == 1 or l[size-1][i] == 1:
            return True
    return False

def second_rule(l):
    size = len(l[0])
    for j in range(size-1):
        for i in range(size-1):
            if l[i][j] == 1 and not (l[i+1][j] == 1 or l[i][j+1] == 1):
                return False
    return True

for i in m:
    if check_zero(i) or (first_rule(i) and second_rule(i)):
        print("YES")
    else:
        print("NO")
