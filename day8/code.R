lines <- readLines("day8/input.txt")
input <- lapply(
    lines,
    function(line){
        digits = strsplit(line, " ")[[1]]
        list(
            signal_patterns = digits[1:10],
            digit_output = digits[12:length(digits)]
        )
    }
)

# part 1
simple_count_signal2digit <- function(signal){
    if(str_length(signal) %in% c(2,4,3,7)){
        1
    }else{
        0
    }
}

belong_simple <- lapply(
    input,
    function(l){
        sapply(l[["digit_output"]], simple_count_signal2digit)
    }
)

sum(unlist(belong_simple)) # 274

# part 2
## helper questions
pattern2abcbinary <- function(pattern){
    str = str_split(pattern,"")[[1]]
    str_code = rep(0,7)
    names(str_code) = letters[1:7]
    str_i = sapply(
        names(str_code),
        function(s){
            if(s %in% str){
                1
            }else{
                0
            }
        }
    )
    str_i
}

simple_abcbinary2digit <- function(abcbinary){
    if(sum(abcbinary)==2){
        "1"
    }else if(sum(abcbinary)==4){
        "4"
    }else if(sum(abcbinary)==3){
        "7"
    }else if(sum(abcbinary)==7){
        "8"
    }else if(sum(abcbinary)==6){
        "0,6,9" # sixs
    }else if(sum(abcbinary)==5){
        "2,3,5" # fives
    }
}

abcbinary2abc <- function(abcbinary){
    names(which(abcbinary > 0))
}

## decode input function
decode_input <- function(input_patterns){
    # input_pattern2abcbinary
    input_pattern2abcbinary <- lapply(
        input_patterns,
        pattern2abcbinary
    )
    names(input_pattern2abcbinary) = input_patterns
    
    # input_pattern2digit (initial)
    input_pattern2digit <- lapply(
        input_pattern2abcbinary,
        simple_abcbinary2digit
    )
    
    # digit2abcbinary function
    digit2abcbinary <- function(digit){
        pattern2abcbinary(names(which(input_pattern2digit==digit)))
    }
    
    # decode sixes
    sixs = input_patterns[input_pattern2digit=="0,6,9"]
    for (six in sixs){
        if(!abcbinary2abc(digit2abcbinary("8") - pattern2abcbinary(six)) %in% 
           abcbinary2abc(digit2abcbinary("1") + digit2abcbinary("4") + digit2abcbinary("7"))){
            input_pattern2digit[[six]] = "9"
        }else{
            if (abcbinary2abc(digit2abcbinary("8") - pattern2abcbinary(six)) %in% abcbinary2abc(digit2abcbinary("4") - digit2abcbinary("1"))){
                input_pattern2digit[[six]] = "0"
            }else{
                input_pattern2digit[[six]] = "6"
            }
            
        }
    }
    
    # seg 6
    seg6 = abcbinary2abc(digit2abcbinary("8") - digit2abcbinary("9"))
    
    # decode fives
    fives = input_patterns[input_pattern2digit=="2,3,5"]
    for (five in fives){
        if (seg6 %in% abcbinary2abc(pattern2abcbinary(five))){
            input_pattern2digit[[five]] = "2"
        }else{
            if (-1 %in% (pattern2abcbinary(five) - digit2abcbinary("1"))){
                input_pattern2digit[[five]] = "5"
            }else{
                input_pattern2digit[[five]] = "3"
            }
        }
    }
    
    # input_abcbinary2digit
    input_abcbinary2digit = input_pattern2digit
    
    names(input_abcbinary2digit) = sapply(
        input_pattern2abcbinary,
        function(abcbinary){
            str_c(abcbinary, collapse = "")
        }
    )
    
    input_abcbinary2digit = unlist(input_abcbinary2digit)
    
    input_abcbinary2digit
}

## decode output function
decode_output <- function(input_patterns, output_patterns){
    #  input_abcbinary2digit
    input_abcbinary2digit = decode_input(input_patterns)
    
    # output_abcbinary
    output_abcbinary = sapply(
        output_patterns,
        function(pattern){
            abcbinary = pattern2abcbinary(pattern)
            str_c(abcbinary, collapse = "")
        }
    )
    
    # output
    output = as.numeric(
        str_c(
            input_abcbinary2digit[output_abcbinary], 
            collapse = ""
        )
    )
    output
}

# # test
# line = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
# digits = strsplit(line, " ")[[1]]
# test = list(
#     signal_patterns = digits[1:10],
#     digit_output = digits[12:length(digits)]
# )
# 
# decode_output(test[["signal_patterns"]], test[["digit_output"]])



# answer
output <- sapply(
    lines,
    function(line){
        digits = strsplit(line, " ")[[1]]
        test = list(
            signal_patterns = digits[1:10],
            digit_output = digits[12:length(digits)]
        )
        decode_output(test[["signal_patterns"]], test[["digit_output"]])
    }
)
names(output) = NULL
sum(output)







