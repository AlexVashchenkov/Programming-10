n = int(input())

l = [input().lower().split(' ') for i in range(n)]

flat_list = [item for sublist in l for item in sublist]

lengths = dict.fromkeys(flat_list, 0)
lengths['polycarp'] = 1

for i in l:
	s1 = i[0]
	s2 = i[2]
lengths[s1] = lengths[s2] + 1

print(max(lengths.values()))