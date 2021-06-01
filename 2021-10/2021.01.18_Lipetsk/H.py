n = int(input())

s = [int(i) for i in input().split(' ')]



def main(s,l):
    for x in l:
        if x == -1:
            continue
        else:
            print(s.index(x)+1, end=" ")
            i = s.index(x)

            s[i] = -1

main(s,sorted(s))
