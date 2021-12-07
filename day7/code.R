lines <- readLines("day7/input.txt")
positions <- as.numeric(strsplit(stringr::str_trim(lines), ",")[[1]])

# part 1
range_sum <- sapply(
    range(positions)[1]:range(positions)[2],
    function(x){
        sum(abs(positions - x))
    }
)
range_sum[which.min(range_sum)] # 355764

# part 2
range_sum2 <- sapply(
    range(positions)[1]:range(positions)[2],
    function(x){
        sum((1+abs(positions - x)) * abs(positions - x) / 2)
    }
)
range_sum2[which.min(range_sum2)]
