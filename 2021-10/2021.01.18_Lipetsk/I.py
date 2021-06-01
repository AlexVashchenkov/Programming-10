from math import *

s = input()
lst = s.split(' ')

n = int(lst[0])
k = int(lst[1])

'''
def power(a, n):
    if n == 0:
        return 1
    if n % 2 == 0:
        return power(a, n / 2) ** 2
    else:
        return a * power(a, n - 1)
'''
def power(a, n):
    return pow(a,n)

def x_n(k):
    return power((n // 2), k)


def y_n_even(k):
    return (power((n // 2), k) - 1)


def y_n_odd(k):
    if k != 0:
        return (n // 2) * (1 - power((n // 2), k)) // (1 - n // 2)
    else:
        return 0


# "n = ", n, "\n", "k = ", k, "\n", x_n(k),y_n_even(k), "sum = ",

if n % 2 == 0:
    print((x_n(k) + y_n_even(k)) % 998244353)
else:
    print((x_n(k) + y_n_odd(k)) % 998244353)