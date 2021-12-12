library(tidyverse)
lines <- readLines("day11/input.txt")

initial_m <- t(sapply(
    lines,
    function(line){
        as.numeric(strsplit(line, "")[[1]])
    }
))
rownames(initial_m) <- NULL

nr = nrow(initial_m)
nc = nrow(initial_m)

index2rc <- function(index, nr){
    if(index %% nr == 0){
        index_r = nr
        index_c = index %/% nr
    }else{
        index_r = index %% nr
        index_c = index %/% nr + 1
    }
    c(index_r, index_c)
}

# part 1
m = initial_m
total_flashes = 0
for (step in 1:100){
    m = m + 1
    
    indexes = which(m > 9)
    flashed_index = indexes
    
    while(length(indexes) > 0){
        rc_list = lapply(indexes, index2rc, nr=nr)
        neighbor_indexes = do.call(
            "bind_rows",
            lapply(
                1:length(rc_list),
                function(j){
                    rc_index=rc_list[[j]]
                    expand_grid(
                        index_r = c(rc_index[1]-1,rc_index[1],rc_index[1]+1),
                        index_c = c(rc_index[2]-1,rc_index[2],rc_index[2]+1)
                    ) %>% 
                        filter(index_r >= 1, index_r <= nr, index_c >= 1, index_c <= nc) %>% 
                        filter(!(index_r==rc_index[1] & index_c==rc_index[2]))
                }
            )
        )
        # single_index = (neighbor_indexes$index_c - 1) * nr + neighbor_indexes$index_r
        # m[single_index] = m[single_index] + 1
        
        for (i in 1:nrow(neighbor_indexes)){
            index_r = neighbor_indexes[i,"index_r"] %>% pull(index_r)
            index_c = neighbor_indexes[i,"index_c"] %>% pull(index_c)
            m[index_r, index_c] = m[index_r, index_c] + 1
        }
        
        indexes = which(m > 9)[!which(m > 9) %in% flashed_index]
        flashed_index = which(m > 9)
    }
    total_flashes = total_flashes + length(flashed_index)
    m[flashed_index] = 0
}

total_flashes # 1725

# part 2
m = initial_m
step=0
while(sum(m) > 0){
    step = step + 1
    m = m + 1
    
    indexes = which(m > 9)
    flashed_index = indexes
    
    while(length(indexes) > 0){
        rc_list = lapply(indexes, index2rc, nr=nr)
        
        neighbor_indexes = do.call(
            "bind_rows",
            lapply(
                1:length(rc_list),
                function(j){
                    rc_index=rc_list[[j]]
                    expand_grid(
                        index_r = c(rc_index[1]-1,rc_index[1],rc_index[1]+1),
                        index_c = c(rc_index[2]-1,rc_index[2],rc_index[2]+1)
                    ) %>% 
                        filter(index_r >= 1, index_r <= nr, index_c >= 1, index_c <= nc) %>% 
                        filter(!(index_r==rc_index[1] & index_c==rc_index[2]))
                }
            )
        )
        
        for (i in 1:nrow(neighbor_indexes)){
            index_r = neighbor_indexes[i,"index_r"] %>% pull(index_r)
            index_c = neighbor_indexes[i,"index_c"] %>% pull(index_c)
            m[index_r, index_c] = m[index_r, index_c] + 1
        }
        
        
        indexes = which(m > 9)[!which(m > 9) %in% flashed_index]
        flashed_index = which(m > 9)
    }
    m[flashed_index] = 0
}

step # 308

