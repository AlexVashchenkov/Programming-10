k = int(input())

for i in range(k):
    n, m, price = map(int, input().split())
    if n - 1 + (m - 1) * n == price:
        print("yes")
    else:
        print("no")
