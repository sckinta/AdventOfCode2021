library(tidyverse)
course_df <- vroom::vroom("day2/input.txt", col_names = c('direction','step'))

# part 1
course_df <- course_df %>% 
    mutate(course_rule=case_when(
        direction=="down" ~ 1,
        direction=="up" ~ -1,
        direction=="forward" ~ 1
    )) %>% 
    mutate(
        course=case_when(
            direction %in% c("down","up") ~ "depth",
            direction == "forward" ~ "horizontal"
        )
    )

positions <- course_df1 %>% 
    group_by(course) %>% 
    summarise(steps = sum(course_rule * step)) %>% 
    deframe()

answer <- positions['depth'] * positions['horizontal']
attr(answer, "names") <- NULL
answer

# part 2
depth=0
aim=0
horizontal=0
for (i in 1:nrow(course_df)){
    direction = course_df %>% 
        dplyr::slice(i) %>% 
        pull(direction)
    step = course_df %>% 
        dplyr::slice(i) %>% 
        pull(step)
    
    if (direction == "up"){
        aim=aim-step
        horizontal = horizontal + 0
    }else if(direction == 'down'){
        aim=aim+step
        horizontal = horizontal + 0
    }else if(direction == 'forward'){
        aim = aim
        horizontal = horizontal + step
        depth = depth + aim * step
    }
}

depth * horizontal


