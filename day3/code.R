input <- readLines("day3/input.txt")

# part 1
bit_m <- str_split(input, "", simplify = T)

detect_common <- function(x){
    bit_str = str_c(x, collapse = "")
    if (str_count(bit_str,"0") > str_count(bit_str,"1")){
        bit = "0"
    }else{
        bit="1"
    }
    bit
}

detect_least <- function(x){
    bit_str = str_c(x, sep = "", collapse = "")
    if (str_count(bit_str,"0") <= str_count(bit_str,"1")){
        bit = "0"
    }else{
        bit="1"
    }
    bit
}


gamma = str_c(apply(bit_m, 2, detect_common), collapse = "")
epsilon = str_c(apply(bit_m, 2, detect_least), collapse = "")

strtoi(gamma, base = 2) * strtoi(epsilon, base = 2) # 852500


# part 2
pos_match <- function(text, pos, pattern){
    str_sub(text, start=pos, end=pos) == pattern
}

start = input
for (i in 1:str_length(input[1])){
    bit_m <- str_split(start, "", simplify = T)
    bit = detect_common(bit_m[,i])
    keep = pos_match(start, i, bit)
    start = start[keep]
    if (length(start)==1){
        break
    }
}
oxygen = start

start = input
for (i in 1:str_length(input[1])){
    bit_m = str_split(start, "", simplify = T)
    bit = detect_least(bit_m[,i])
    keep = pos_match(start, i, bit)
    start = start[keep]
    if (length(start)==1){
        break
    }
}
co2 = start

strtoi(oxygen, base=2) * strtoi(co2, base=2) # 1007985



