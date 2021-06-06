n = int(input())

speed = 0
speed_limits = []
overtake = 0

ignore = 0

for i in range(n):
    event = list(input().split())
    if (len(event) == 2):
        if int(event[0]) == 1:
            speed = int(event[1])
            while len(speed_limits) > 0 and speed_limits[-1] < speed:
                ignore += 1
                speed_limits.pop()

        else:
            if int(event[1]) < speed:
                ignore += 1
            else:
                speed_limits.append(int(event[1]))
    else:
        if int(event[0]) == 2:
            ignore += overtake
            overtake = 0
        elif int(event[0]) == 4:
            overtake = 0
        elif int(event[0]) == 5:
            speed_limits = []
        else:
            overtake += 1

print(ignore)