# part 1
inputfile="day1/input.txt"
l <- as.numeric(readLines(inputfile))

count_increased_number <- function(l){
    sum(l - dplyr::lag(l) > 0, na.rm = T)
}

count_increased_number(l)

# part 2
calculate_sum <- function(i, n=3){
    sum(l[i:(i+n-1)])
}
l <- as.numeric(readLines(inputfile))
start_i <- 1
end_i <- length(l)-(n-1)
sum_l <- sapply(
    start_i:end_i,
    calculate_sum
)
count_increased_number(sum_l)
