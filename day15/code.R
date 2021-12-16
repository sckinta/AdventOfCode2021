library(tidyverse)
library(stringr)

lines = readLines("day15/input.txt")
input_m = map(lines, ~as.numeric(str_split(.x,"")[[1]])) %>% 
    do.call("rbind", .)

index2xy <- function(index, nr){
    if(index %% nr == 0){
        x = nr
        y = index %/% nr
    }else{
        x = index %% nr
        y = index %/% nr + 1
    }
    c(x,y)
}

# part 1
xmax = dim(input_m)[2]#
ymax = dim(input_m)[1]# 

# start_index=c(1,1)
# scores = 0

n = xmax*ymax

## Dijkstra : https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm#Algorithm
current_index = 1
unvisited_set=1:n
dist_m=c(0, rep(Inf, n-1))

while(n %in% unvisited_set || length(unvisited_set)!=0){
    current_node=index2xy(current_index, ymax)
    unvisited_set = unvisited_set[current_index!=unvisited_set]
    neighbor_nodes = tibble(
        x=c(current_node[1]-1,current_node[1],current_node[1],current_node[1]+1),
        y=c(current_node[2], current_node[2]+1, current_node[2]-1, current_node[2])
    ) %>% 
        filter(x > 0, x <= xmax, y > 0, y <= ymax) %>% 
        mutate(value = map2_dbl(x,y, ~input_m[.x,.y])) %>% 
        mutate(index = x+(y-1)*xmax) %>% 
        filter(index %in% unvisited_set) %>% 
        mutate(dist = dist_m[current_index] + value) %>% 
        mutate(dist = map2_dbl(dist, index, ~ifelse(.x < dist_m[.y], .x, dist_m[.y])))
    dist_m[neighbor_nodes$index] = neighbor_nodes$dist
    current_index = unvisited_set[which.min(dist_m[unvisited_set])]
}
dist_m[n] # 435


# ## list all possible
# grow_steps <- function(selected, x_avail, y_avail){
#     if(x_avail+y_avail == 0){
#         return(list(selected))
#     }else{
#         
#         if(selected=="x"){
#             x_avail = x_avail - 1
#         }
#         if(selected=="y"){
#             y_avail = y_avail - 1
#         }
# 
#         next_possible <- c("x", "y")
#         
#         if(x_avail==0 && y_avail!=0){
#             next_possible <- str_c(rep("y", y_avail), collapse = "")
#             y_avail = 0
#         }
#         if(y_avail==0 && x_avail!=0){
#             next_possible <- str_c(rep("x", x_avail), collapse = "")
#             x_avail = 0
#         }
#         
#         map(next_possible, grow_steps, x_avail=x_avail, y_avail=y_avail) %>% 
#             do.call(c,.) %>% 
#             map(~str_c(c(selected, .), collapse = ""))
#     }
# }
# 
# possible_steps = c(
#     grow_steps("y", xmax, ymax),
#     grow_steps("x", xmax, ymax)
# )
# 
# # y is towards down, x is towards right
# start_index = c(1, 1)
# 
# get_path <- function(start_index, path){
#     if(str_sub(path,start=1,end=1) == "y"){
#         start_index = c(start_index[1]+1, start_index[2])
#     }else{
#         start_index = c(start_index[1], start_index[2]+1)
#     }
#     if(str_length(path)==1){
#         return(list(start_index))
#     }
#     path = str_sub(path,start=2,end=-1)
#     c(get_path(start_index, path),list(start_index))
# }
# 
# get_path_score <- function(path){
#     get_path(start_index, path) %>% 
#         rev() %>% 
#         map_dbl(~input_m[.x[1],.x[2]]) %>% 
#         sum()
# }
# 
# possible_scores = map_dbl(possible_steps, get_path_score)
# min(possible_scores)

# part 2
## generate new
tiles = vector(mode = "list", 25)
tiles[[1]] = input_m

for (i in 2:25){
    double_i = index2xy(i, 5)
    if(double_i[1]!=1){
        prev_i = double_i[1]-1 + (double_i[2]-1)*5
    }else{
        prev_i = i-5
    }
    tiles[[i]] = tiles[[prev_i]] + 1
    tiles[[i]][which(tiles[[i]] > 9)] = 1
}


new_input = lapply(
    0:4,
    function(col){
        map((1:5)+col*5, ~tiles[[.x]]) %>% 
            do.call("rbind",.)
    }
) %>% 
    do.call("cbind",.)
rm(tiles)

xmax = dim(new_input)[1]
ymax = dim(new_input)[2]

n = xmax*ymax
current_index = 1
unvisited_set=1:n
dist_m=c(0, rep(Inf, n-1))

while(n %in% unvisited_set || length(unvisited_set)!=0){
    current_node=index2xy(current_index, ymax)
    unvisited_set = unvisited_set[current_index!=unvisited_set]
    neighbor_nodes = tibble(
        x=c(current_node[1]-1,current_node[1],current_node[1],current_node[1]+1),
        y=c(current_node[2], current_node[2]+1, current_node[2]-1, current_node[2])
    ) %>% 
        filter(x > 0, x <= xmax, y > 0, y <= ymax) %>% 
        mutate(value = map2_dbl(x,y, ~new_input[.x,.y])) %>% 
        mutate(index = x+(y-1)*xmax) %>% 
        filter(index %in% unvisited_set) %>% 
        mutate(dist = dist_m[current_index] + value) %>% 
        mutate(dist = map2_dbl(dist, index, ~ifelse(.x < dist_m[.y], .x, dist_m[.y])))
    dist_m[neighbor_nodes$index] = neighbor_nodes$dist
    current_index = unvisited_set[which.min(dist_m[unvisited_set])]
}

dist_m[n] # 
