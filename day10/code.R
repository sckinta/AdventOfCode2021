library(stringr)
library(tidyverse)
lines = readLines("day10/input.txt")

# part 1
possible_errors <- expand_grid(
    start=c("\\(","\\[","\\{","<"),
    end=c("\\)","\\]","\\}",">")
) %>% 
    mutate(pattern=glue::glue("{start}{end}")) %>% 
    mutate(pattern=as.character(pattern)) %>% 
    filter(!pattern %in% c("\\(\\)", "\\[\\]", "\\{\\}", "<>")) %>% 
    mutate(point=case_when(
        end=="\\)" ~ 3,
        end=="\\]" ~ 57,
        end=="\\}" ~ 1197,
        end==">" ~ 25137,
    ))

extract_corrupt_error_point <- function(line){
    point = 0
    for (i in 1:(str_length(line)/2)){
        line = gsub("\\(\\)","",line)
        line = gsub("\\[\\]","",line)
        line = gsub("\\{\\}","",line)
        line = gsub("<>","",line)
        bl = sapply(possible_errors$pattern, function(pattern){str_detect(line, pattern)})
        if (any(bl)){
            point = possible_errors %>% dplyr::slice(which(bl)) %>% pull(point)
            break
        }
    }
    point
}

error_points = sapply(lines, extract_corrupt_error_point)
sum(error_points) # 392139

# part 2
point = 0
get_remain_line <- function(line){
    for (i in 1:(str_length(line)/2)){
        line = gsub("\\(\\)","",line)
        line = gsub("\\[\\]","",line)
        line = gsub("\\{\\}","",line)
        line = gsub("<>","",line)
        line
    }
    line
}

remain_lines = sapply(lines, get_remain_line)
remain_lines = remain_lines[error_points==0]
names(remain_lines) = NULL

get_completion_point <- function(remain_line){
    line_split=str_split(remain_line,"")[[1]]
    point=0
    for (i in str_length(remain_line):1){
        if(line_split[i]=="("){
            point = point * 5 + 1
        }else if(line_split[i]=="["){
            point = point * 5 + 2
        }else if(line_split[i]=="{"){
            point = point * 5 + 3
        }else if(line_split[i]=="<"){
            point = point * 5 + 4
        }
    }
    point
}

incompletion_score = sapply(remain_lines, get_completion_point)
median(incompletion_score) # 4001832844





