library(stringr)
library(tidyverse)
lines <- readLines("day5/input.txt")

# part 1
extract_line_segment <- function(line){
    coords = strsplit(str_replace_all(line, " ",""), split="->")[[1]]
    start = strsplit(coords[1], split=",")[[1]]
    start = as.numeric(start)
    end = strsplit(coords[2], split=",")[[1]]
    end = as.numeric(end)
    if (start[1]==end[1]){
        x = rep(start[1], abs(start[2]-end[2])+1)
        y = start[2]:end[2]
    }else if (start[2]==end[2]){
        x = start[1]:end[1]
        y = rep(start[2], abs(start[1]-end[1])+1)
    }else{
        x = NULL
        y = NULL
    }
    tibble(x = x, y = y)
}

index_coords = do.call(
    "bind_rows",
    lapply(lines, extract_line_segment)
)

index_coords = index_coords %>% 
    group_by(x, y) %>% 
    count() %>% 
    ungroup()

index_coords %>% 
    filter(n > 1) %>% 
    nrow() # 5690

# part 2
extract_line_segment_withDiag <- function(line){
    coords = strsplit(str_replace_all(line, " ",""), split="->")[[1]]
    start = strsplit(coords[1], split=",")[[1]]
    start = as.numeric(start)
    end = strsplit(coords[2], split=",")[[1]]
    end = as.numeric(end)
    if (start[1]==end[1]){
        x = rep(start[1], abs(start[2]-end[2])+1)
        y = start[2]:end[2]
    }else if (start[2]==end[2]){
        x = start[1]:end[1]
        y = rep(start[2], abs(start[1]-end[1])+1)
    }else if (abs(start[1]-end[1]) == abs(start[2] - end[2])) {
        x = start[1]:end[1]
        y = start[2]:end[2]
    } else{
        x = NULL
        y = NULL
    }
    tibble(x = x, y = y)
}

index_coords = do.call(
    "bind_rows",
    lapply(lines, extract_line_segment_withDiag)
)

index_coords = index_coords %>% 
    group_by(x, y) %>% 
    count() %>% 
    ungroup()

index_coords %>% 
    filter(n > 1) %>% 
    nrow() # 17741
