library(stringr)
library(tidyverse)

lines = readLines("day14/input.txt")
template = lines[1]


# part 1
rules = lines[3:length(lines)] %>% 
    map_chr( ~ str_split(.x, " -> ")[[1]][2])
names(rules) = lines[3:length(lines)] %>% 
    map_chr( ~ str_split(.x, " -> ")[[1]][1])

elements = map(names(rules), ~str_split(.x,"")[[1]]) %>% 
    flatten() %>% 
    unlist() %>% 
    append(rules) %>% 
    unique()

polymerize <- function(input){
    output = map(
        1:(str_length(input)-1),
        function(i){
            element=rules[str_sub(input, start = i, end = i+1)]
            str_c(str_sub(input, start = i, end = i), element, collapse = "")
        }
    )
    output = append(output, str_sub(input, start = str_length(input), end = str_length(input)))
    str_c(output, collapse = "")
}

input = template
for (step in 1:10){
    input = polymerize(input)
}

element_count = map_int(elements, ~str_count(input, .x)) %>% 
    set_names(elements)

max(element_count) - min(element_count) # 2899

# part 2
input = template
 
rules = map_dfr(
    lines[3:length(lines)],
    function(line){
        line = str_split(line, " -> ")[[1]]
        elements = str_split(line[1],"")[[1]]
        tibble(
            from = rep(line[1],2),
            to = c(
                str_c(elements[1],line[2], collapse = ""),
                str_c(line[2],elements[2], collapse = "")
            )
        )
    }
)

elements = unique(c(rules$from, rules$to))


element_counts = map_int(elements, ~str_count(template, .x)) %>% 
    set_names(elements)
element_counts = element_counts[element_counts > 0]
# double_counts = map_int(elements, ~str_count(input, .x)) %>% 
#     set_names(elements)

for (step in 1:40){
    element_counts = rules %>% 
        filter(from %in% names(element_counts)) %>% 
        left_join(
            enframe(element_counts,name="from")
        ) %>% 
        group_by(to) %>% 
        summarise(value=sum(value)) %>% 
        ungroup() %>% 
        deframe()
}

# convert double element count to single element count
single_elements = map(elements, ~str_split(.x,"")[[1]]) %>% 
    flatten() %>% unique() %>% unlist()

convert_double2single <- function(double_counts){
    single_counts = map_dbl(
        single_elements,
        function(s){
            sum(map_dbl(names(double_counts), ~str_count(.x, s)*double_counts[.x]))
        }
    ) %>% 
        set_names(single_elements)
    template_len = str_length(template)
    single_counts[str_sub(template, 1, 1)] = single_counts[str_sub(template, 1, 1)] + 1
    single_[jkhlcounts[str_sub(template, template_len, template_len)] = single_counts[str_sub(template, template_len, template_len)] + 1
    single_counts = single_counts / 2
    single_counts
}

single_counts = convert_double2single(element_counts)

(max(single_counts) - min(single_counts)) %>% format(scientific=FALSE)

# s="NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB"
# map_int(single_elements, ~str_count(s,.x)) %>% 
#     set_names(single_elements)






