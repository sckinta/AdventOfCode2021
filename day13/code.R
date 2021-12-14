library(tidyverse)
lines=readLines("day13/input.txt")

## extract input
coords=tibble(x=integer(), y=integer())
insts=tibble(direction=character(), coord=integer())
for (line in lines){
    if(grepl(",",line)){
        coord = as.numeric(str_split(line,",")[[1]])
        coords = bind_rows(
            coords,
            tibble(x=coord[1],y=coord[2])
        )
    }else if(grepl("fold", line)){
        inst = str_split(line," ")[[1]][3]
        inst = str_split(inst,"=")[[1]]
        insts = bind_rows(
            insts,
            tibble(
                direction = inst[1],
                coord = as.numeric(inst[2])
            )
        )
    }
}

# coords = coords %>% 
#     mutate(
#         x = x + 1,
#         y = y+1
#     )


get_fold_coords <- function(coords_folded, fold_axis, fold_coord){
    if (fold_axis=="y"){
        coords_folded = coords_folded %>% 
            mutate(
                y = ifelse(
                    y > fold_coord, 
                    fold_coord - (y-fold_coord), y
                )
            )
    }else{
        coords_folded = coords_folded %>% 
            mutate(
                x = ifelse(
                    x > fold_coord, 
                    fold_coord - (x-fold_coord), x
                )
            )
    }
    coords_folded %>% distinct(x,y)
}



# part 1
fold_axis = insts %>% dplyr::slice(1) %>% pull(direction)
fold_coord = insts %>% dplyr::slice(1) %>% pull(coord)
coords_folded = get_fold_coords(coords, fold_axis, fold_coord)
coords_folded %>% distinct(x,y) %>% nrow() # 716

# part 2
coords_folded = coords
xmax = max(coords_folded$x)
ymax = max(coords_folded$y)

for (i in 1:nrow(insts)){
    fold_axis = insts %>% dplyr::slice(i) %>% pull(direction)
    fold_coord = insts %>% dplyr::slice(i) %>% pull(coord)
    coords_folded = get_fold_coords(coords_folded, fold_axis, fold_coord)
    if (fold_axis == "y"){
        ymax = ymax - fold_coord -1
        if (ymax - fold_coord > fold_coord){
            coords_folded = coords_folded %>% 
                mutate(y = y + ymax - (fold_coord - 1))
        }
    }else if (fold_axis == "x"){
        xmax = xmax - fold_coord -1
        if (xmax - fold_coord > fold_coord){
            coords_folded = coords_folded %>% 
                mutate(x = x + xmax - (fold_coord - 1))
        }
    }
}

final_m = expand_grid(
    x = 0:xmax,
    y = 0:ymax
) %>% 
    left_join(
        coords_folded %>% 
            mutate(value = 1)
    ) %>% 
    mutate(value=ifelse(is.na(value),0, value))

final_m %>% 
    mutate(y=ymax - y) %>% 
    ggplot(aes(x=x, y=y, fill=value)) +
    geom_tile()
# RPCKFBLR
