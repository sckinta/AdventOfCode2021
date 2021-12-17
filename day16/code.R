library(stringr)
library(tidyverse)

hexa2binary = c(
    "0" = "0000",
    "1" = "0001",
    "2" = "0010",
    "3" = "0011",
    "4" = "0100",
    "5" = "0101",
    "6" = "0110",
    "7" = "0111",
    "8" = "1000",
    "9" = "1001",
    "A" = "1010",
    "B" = "1011",
    "C" = "1100",
    "D" = "1101",
    "E" = "1110",
    "F" = "1111"
)

binary2literal = c(
    "0000"= 0,
    "0001"= 1,
    "0010"= 2,
    "0011"= 3,
    "0100"= 4,
    "0101"= 5,
    "0110"= 6,
    "0111"= 7,
    "1000"= 8,
    "1001"= 9,
    "1010"= 10,
    "1011"= 11,
    "1100"= 12,
    "1101"= 13,
    "1110"= 14,
    "1111"= 15
)



binary2num = c(
    "000"="0",
    "001"="1",
    "010"="2",
    "011"= "3",
    "100" = "4",
    "101" = "5",
    "110" = "6",
    "111" = "7"
)

convert_hexa2binary <- function(hexa){
    hexa_l = str_split(hexa,"")[[1]]
    map_chr(hexa_l, ~hexa2binary[.x]) %>% 
        str_c(.,collapse = "")
}

binary = convert_hexa2binary("38006F45291200")

translate_binary <- function(binary, totalPackets=Inf){
    if(str_length(binary) == 3){
        return(binary)
    }
    if (str_length(binary)==0){
        return(NULL)
    }
    if(totalPackets == 0){
        return(NULL)
    }
    totalPackets = totalPackets - 1
    # header
    version = str_sub(binary,1,3)
    typeId = translate_number(str_sub(binary,4,6))
    str_sub(binary, 1, 6) = ""
    # literal value packet -> single binary number
    if (typeId == 4){
        groups = identify_literal_group(binary)
        if(str_length(groups[[length(groups)]]) >= 5){
            iteral_groups = groups[-length(groups)]
            iteral_groups = c(iteral_groups, str_sub(groups[[length(groups)]],1,5))
            binary = groups[[length(groups)]]
            str_sub(binary, 1, 5)=""
        }else{
            binary = groups[[length(groups)]]
        }
    }else{
        lenTypeId=str_sub(binary, 1, 1)
        str_sub(binary, 1, 1)=""
        if (lenTypeId=="0"){
            totalPackets=Inf
            totalLen = str_sub(binary, 1, 15)
            str_sub(binary, 1, 15)=""
            totalLen = translate_number(totalLen)
            binary = str_sub(binary, 1, totalLen)
        }else if(lenTypeId=="1"){
            totalPackets = str_sub(binary, 1, 11)
            str_sub(binary, 1, 11) = ""
            totalPackets = translate_number(totalPackets)
        }
    }
    map(
        c(version, binary),
        translate_binary,
        totalPackets=totalPackets
    ) %>% 
        do.call(c,.)
}

translate_binary("00111000000000000110111101000101001010010001001000000000")
translate_binary("11101110000000001101010000001100100000100011000001100000")
translate_binary("110100101111111000101000")

binary = convert_hexa2binary("620080001611562C8802118E34")
map(
    translate_binary(convert_hexa2binary("8A004A801A8002F478")),
    translate_number
)


identify_literal_group <- function(binary){
    if(str_sub(binary,1,1)=="0" || str_length(binary) <= 5){
        return(list(binary))
    }else{
        group_i = str_sub(binary, 1, 5)
        str_sub(binary, 1, 5) = ""
        map(
            c(group_i, binary),
            identify_literal_group
        ) %>% 
            do.call(c,.)
    }
}

translate_literal_groups <- function(groups){
    binaryNum = map_chr(
        1:length(groups), 
        function(i){
            bit5 = str_sub(packet, 1+(i-1)*5, 1+i*5-1)
            binary2literal[str_sub(bit5, 2,5)]
        }
    )
    binaryNum = rev(binaryNum)
    literalVal = sum(map_dbl(1:length(binaryNum), ~16^(.x-1) * as.numeric(binaryNum[.x])))
    literalVal
}

translate_number <- function(binaryNum){
    while(str_length(binaryNum) %% 3 != 0){
        binaryNum = gsub("^0", "",binaryNum)
    }
    part_n = str_length(binaryNum) %/% 3
    binaryNum = map_dbl(1:part_n, ~as.numeric(binary2num[str_sub(binaryNum, 1+(.x-1)*3, .x*3)]))
    binaryNum = rev(binaryNum)
    binaryNum = sum(map_dbl(1:part_n, ~8^(.x-1)*binaryNum[.x]))
    binaryNum
}




