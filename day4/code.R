library(stringr)

lines = readLines("day4/input.txt")

number_list = as.numeric(unlist(str_split(lines[1],",")))

boards=vector(mode = "list", length = (length(lines)-1)/6)
singleBoard = matrix(0,5,5)
for (i in 2:length(lines)){
    if ((i-2) %% 6 == 0){
        next
    }else{
        row_i = (i-2) %% 6
        board_i = (i-2) %/% 6 + 1
        row_v = as.numeric(unlist(str_split(str_trim(lines[i]), "\\s+")))
        singleBoard[row_i,] = row_v
        if (row_i == 5){
            boards[[board_i]] = singleBoard
            singleBoard = matrix(0,5,5)
        }
    }
}

boards_mark=rep(list(matrix(F,5,5)),length(boards))
win_board=0
score = 0
for (num in number_list){
    for (board_i in 1:length(boards)){
        if (num %in% boards[[board_i]]){
            i = which(boards[[board_i]]==num)
            col_i = (i-1) %/% 5 + 1
            row_i = i %% 5
            if (row_i==0){
                row_i = 5
            }
            boards_mark[[board_i]][row_i, col_i] = T
        }
        win_row =  which(5==apply(boards_mark[[board_i]], 1, sum))
        win_col = which(5==apply(boards_mark[[board_i]], 2, sum))
        if ( length(win_row)!=0 || length(win_col)!=0){
            win_board = board_i
            unmark_sum = sum(boards[[board_i]][!(boards_mark[[board_i]])])
            score = num * unmark_sum
            break
        }
    }
    if (win_board!=0){
        break
    }
}

win_board # 23
score # 38594

# part 2
boards_mark=rep(list(matrix(F,5,5)),length(boards))
win_boards=rep(F, length(boards))
score = 0
for (num in number_list){
    for (board_i in 1:length(boards)){
        if (num %in% boards[[board_i]]){
            i = which(boards[[board_i]]==num)
            col_i = (i-1) %/% 5 + 1
            row_i = i %% 5
            if (row_i==0){
                row_i = 5
            }
            boards_mark[[board_i]][row_i, col_i] = T
        }
        win_row =  which(5==apply(boards_mark[[board_i]], 1, sum))
        win_col = which(5==apply(boards_mark[[board_i]], 2, sum))
        if ( length(win_row)!=0 || length(win_col)!=0){
            win_boards[board_i] = T
            if (sum(win_boards)==length(boards)){
                # win_board = board_i
                unmark_sum = sum(boards[[board_i]][!(boards_mark[[board_i]])])
                score = num * unmark_sum
                break
            }
        }
    }
    if (sum(win_boards)==length(boards)){
        break
    }
}

score # 21184