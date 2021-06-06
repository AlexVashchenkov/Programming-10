n = int(input())

s = [input() for i in range(n)]

def add(n):
    for i in range(len(n)):
        n = n[0:i] + str(int(n[i]) + 1) + n[i+1:]
    return n

for i in range(n):
    print(len(add(n)) % 1000000007)
