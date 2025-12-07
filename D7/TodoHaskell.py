with open("./input.txt", "r") as f:
    inp = list(map(lambda x: x.strip(), f.readlines()))

m, n = len(inp), len(inp[0])

dp = [[0 for _ in range(n)] for _ in range(m)]
for i in range(n):
    if inp[0][i] == 'S':
        dp[0][i] = 1
        break

for i in range(1, m):
    for j in range(n):
        if inp[i][j] == '.':
            if j - 1 >= 0 and inp[i][j - 1] == '^':
                dp[i][j] += dp[i][j - 1]
            if j + 1 < n and inp[i][j + 1] == '^':
                dp[i][j] += dp[i][j + 1]
            if i - 1 >= 0 and inp[i - 1][j] != '^':
                dp[i][j] += dp[i - 1][j]
        else:
            if i - 1 > 0 and inp[i - 1][j] in ['.', 'S']:
                dp[i][j] += dp[i - 1][j]
            if j - 1 >= 0:
                dp[i][j - 1] += dp[i][j]

print(*dp, sep="\n")
print(sum(dp[-1]))
