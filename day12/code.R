library(stringr)
library(tidyverse)

lines = readLines("day12/input.txt")
lines = readLines("day12/test.txt")

lines = do.call(
    "bind_rows",
    lapply(
        lines,
        function(line){
            line = str_split(line,"-")[[1]]
            tibble(from = line[1], to=line[2])
        }
    )
)
    
lines = lines %>% 
    mutate(
        from_new=case_when(
            from=="end" ~ to,
            to=="start" ~ to,
            TRUE ~ from
        )
    ) %>% 
    mutate(
        to_new=case_when(
            to=="start" ~ from,
            from == "end" ~ from,
            TRUE ~ to
        )
    ) %>% 
    select(from_new, to_new) %>% 
    dplyr::rename(from=from_new, to=to_new)


lines = lines %>% 
    bind_rows(
        lines %>% 
            filter(from!="start", to!="end") %>% 
            select(to=from, from=to) 
    )

connectors = lines %>% 
    arrange(from) %>% 
    group_by(from) %>% 
    summarise(to=list(to)) %>% 
    deframe()

# node = "start"
# possible_next = connectors[[node]]
# paths = lapply(
#     possible_next,
#     function(next_node){
#         c(node, next_node)
#     }
# )
# used_little = lapply(
#     possible_next,
#     function(next_node){
#         if(str_detect(next_node,"[[:lower:]]")){
#             next_node
#         }else{
#             NULL
#         }
#     }
# )
#     
# node = "HN"
# possible_next = connectors[[node]]


### code from David Robinson - recursive function
# more recursive function https://data-flair.training/blogs/r-recursive-function/
bfs <- function(node, visited_small=NULL, part1=FALSE){
    if(node=="end"){
        return(list(node))
    }
    
    if(node=="start" && "start" %in% visited_small){
        return(NULL)
    }
    
    if(str_detect(node, "[[:lower:]]")){
        visited_small = c(visited_small, node)
    }
    
    possible_next <- connectors[[node]]
    
    if(part1 || any(duplicated(visited_small))){
        possible_next <- setdiff(possible_next, visited_small)
    }
    
    map(possible_next, bfs, visited_small=visited_small, part1=part1) %>%
        do.call(c, .) %>%
        map(~c(node, .))
}

# part 1
length(bfs("start", part1=T)) # 4167

# part 2
length(bfs("start", part1=F)) # 98441
