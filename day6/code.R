input <- readLines("day6/input.txt")

# part 1
reset_seq <- function(seq){
    if (any(seq == -1)){
        new_add = rep(8,sum(seq == -1))
        seq[which(seq == -1)]=6
        seq = c(seq, new_add)
    }
    seq
}

days <- 80
initial_timer <- as.numeric(strsplit(input[1], ",")[[1]])
seq <- initial_timer
for (day in 1:days){
    seq <- reset_seq(seq - 1) 
}

length(seq) # 352872

# part 2
initial_timer <- as.numeric(strsplit(input[1], ",")[[1]])
counts <- c(0,tabulate(initial_timer, nbins=8))

days=256
for (i in 1:days) {
    n0 <- counts[1]
    tmp <- counts[-1]
    tmp[7] = tmp[7] + n0
    tmp[9] = n0
    counts <- tmp
}
format(sum(counts), scientific=F) # 1604361182149

