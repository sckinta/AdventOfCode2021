lines <- readLines("day9/input.txt")

input_matrix <- t(sapply(
    lines,
    function(line){
        as.numeric(strsplit(line,"")[[1]])
    }
))
rownames(input_matrix) = NULL

nr = dim(input_matrix)[1]
nc = dim(input_matrix)[2]

# part 1
low_points = c()
for (i in 1:length(input_matrix)){
    r_index = i %% nr
    if(r_index==0){
        r_index = nr
    }
    if (i == length(input_matrix)){
        c_index = i %/% nr 
    }else{
        c_index = i %/% nr + 1
    }
    
    
    neighbors_index = list(
        up_index = c(r_index -1, c_index),
        down_index = c(r_index + 1, c_index),
        left_index = c(r_index, c_index - 1),
        right_index = c(r_index, c_index + 1)
    )
    
    neighbors = sapply(
        neighbors_index,
        function(index){
            if (index[1] < 1 | index[1] > nr | index[2] < 1 | index[2] > nc){
                NA
            }else{
                input_matrix[index[1], index[2]]
            }
        }
    )
    if (all(input_matrix[r_index, c_index] < neighbors, na.rm = T)){
        low_points = c(low_points, input_matrix[r_index, c_index])
    }
}

sum(sapply(low_points, function(x){x + 1})) # 504

# part 2
low_point_index= data.frame(
    r_index=integer(),
    c_index=integer()
)
for (i in 1:length(input_matrix)){
    r_index = i %% nr
    if(r_index==0){
        r_index = nr
    }
    if (i == length(input_matrix)){
        c_index = i %/% nr 
    }else{
        c_index = i %/% nr + 1
    }
    
    
    neighbors_index = list(
        up_index = c(r_index -1, c_index),
        down_index = c(r_index + 1, c_index),
        left_index = c(r_index, c_index - 1),
        right_index = c(r_index, c_index + 1)
    )
    
    neighbors = sapply(
        neighbors_index,
        function(index){
            if (index[1] < 1 | index[1] > nr | index[2] < 1 | index[2] > nc){
                NA
            }else{
                input_matrix[index[1], index[2]]
            }
        }
    )
    if (all(input_matrix[r_index, c_index] < neighbors, na.rm = T)){
        low_point_index = rbind(
            low_point_index, 
            data.frame(r_index=r_index, c_index=c_index)
        )
    }
}

# index2rc <- function(i, nr, nc){
#     r_index = i %% nr
#     if(r_index==0){
#         r_index = nr
#     }
#     if (i == nr * nc){
#         c_index = i %/% nr 
#     }else{
#         c_index = i %/% nr + 1
#     }
#     c(r_index=r_index, c_index=c_index)
# }


get_neighbor_index <- function(r_index, c_index){
    neighbors_index = list(
        up_index = c(r_index -1, c_index),
        down_index = c(r_index + 1, c_index),
        left_index = c(r_index, c_index - 1),
        right_index = c(r_index, c_index + 1)
    )
    
    neighbors_index = do.call(
        "rbind",
        lapply(
            neighbors_index,
            function(index){
                if (index[1] < 1 | index[1] > nr | index[2] < 1 | index[2] > nc){
                    data.frame(
                        r_index=index[1],
                        c_index=index[2],
                        value=NA
                    )
                }else if(input_matrix[index[1], index[2]]==9){
                    data.frame(
                        r_index=index[1],
                        c_index=index[2],
                        value=NA
                    )
                }else{
                    data.frame(
                        r_index=index[1],
                        c_index=index[2],
                        value=input_matrix[index[1], index[2]]
                    )
                }
            }
        )
    )
    
    neighbors_index = neighbors_index[!is.na(neighbors_index[,"value"]),]
    rownames(neighbors_index)=NULL
    neighbors_index
}

count_basin_size <- function(r_index, c_index){
    basin = data.frame(
        r_index=r_index,
        c_index=c_index,
        value=input_matrix[r_index, c_index]
    )
    i = 1
    while (i <= nrow(basin)){
        r_index = basin[i,"r_index"]
        c_index = basin[i,"c_index"]
        neighbor_index = get_neighbor_index(r_index, c_index)
        basin = rbind(basin, neighbor_index)
        basin = unique(basin)
        i=i+1
    }
    nrow(basin)
}

basin_size = sapply(
    1:nrow(low_point_index),
    function(x){
        r_index=low_point_index[x,"r_index"]
        c_index=low_point_index[x,"c_index"]
        count_basin_size (r_index, c_index)
    }
)

basin_size = sort(basin_size, decreasing=T)
basin_size[1] * basin_size[2] * basin_size[3] # 1558722


