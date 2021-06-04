t = int(input())
 
for i in range(t):
    n = int(input())
    creams = [int(i) for i in input().split(' ')]

    cream = 0
    layers = [0] * len(creams)
    for i in range(len(layers) - 1, -1, -1):
        cream = max(cream, creams[i])
        if cream > 0:
            layers[i] = 1
            cream -= 1
    print(*layers)
