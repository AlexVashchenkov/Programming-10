n = int(input())

nums = list(map(int, input().split()))
counts = [0] * 100005

for num in nums:
    counts[num] += 1

dp = [0] * 100001

dp[0] = 0
dp[1] = counts[1] * 1

for i in range(2, len(dp)):
    dp[i] = max(counts[i] * i + dp[i - 2], dp[i - 1])

print(dp[len(dp) - 1])