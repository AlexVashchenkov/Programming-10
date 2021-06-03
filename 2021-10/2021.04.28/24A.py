n = int(input())
m = [[j, [int(i) for i in input().split()]] for j in range(n)]
k = m
h = [k[0]]
k.pop(0)
end = h[0][1][1]


def search_for_next(end):
    for j in range(len(k)):
        if end == k[j][1][0]:
            h.append(k[j])
            k.pop(j)
            break
        elif end == k[j][1][1]:
            k[j][1][2] = k[j][1][2] * (-1)
            k[j][1][1] = k[j][1][0]
            k[j][1][0] = end
            h.append(k[j])
            k.pop(j)
            break


for i in range(n):
    search_for_next(end)
    end = h[i][1][1]

onesidecost = 0
othersidecost = 0

for i in range(n):
    if h[i][1][2] > 0:
        onesidecost += h[i][1][2]
    else:
        othersidecost += h[i][1][2] * (-1)

print(min(onesidecost, othersidecost))
