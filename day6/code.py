with open("day6/input.txt", "r") as fh:
    lines = fh.readlines()

initial_timer = lines[0].rstrip().replace(" ","").split(",")
initial_timer = [int(i) for i in initial_timer]

# part 1
def reset_seq(seq):
    reset_bl = [i==-1 for i in seq]
    if (any(reset_bl)):
        reset_n = sum(reset_bl)
        for i,x in enumerate(reset_bl):
            if x:
                seq[i] = 6
        seq = seq + [8] * reset_n
    return seq

days = 80
seq = initial_timer
for i in range(0,days):
    seq = reset_seq([x-1 for x in seq])

len(seq) # 352872


# part 2
counts=[]
for i in range(0,9):
    bl = [d==i for d in initial_timer]
    counts.append(sum(bl))

counts

for i in range(0,256):
    n0 = counts[0]
    tmp = counts[1:9]
    tmp[6] = tmp[6] + n0
    tmp.append(n0)
    counts = tmp
    
sum(counts) # 1604361182149
