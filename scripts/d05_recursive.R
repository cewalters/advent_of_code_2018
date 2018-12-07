# find the zeros

ex <- "dabAcCaCBAcCcaDA"
ex_char <- unlist(strsplit(ex, split = ""))
ex_vec <- unname(vapply(ex_char, letter2integer, numeric(1)))

shortest_polymer(ex_vec)


#########################################################
# dat <- readLines("data/d05_1.txt", n = -1L)
# dat_char_vec <- unlist(strsplit(dat, split = ""))
# dat_vec <- unname(vapply(dat_char_vec, letter2integer, numeric(1)))
# save(dat_vec, file = "data/d05_1_numeric.Rds")

# read in dat_vec
load(file = "data/d05_1_numeric.Rds")
remove_pairs_recursive(dat_vec)
shortest_polymer(dat_vec)

###########################################################
# functions

letter2integer <- function(character_letter){
  df <- data.frame("letters" = c(letters, LETTERS), 
                   "value" = c(seq(1:26), seq(-1, -26)))
  df[ which(df$letters == character_letter), "value" ]
}


midpoint <- function(vec){
  vec[-length(vec)] + diff(vec)/2
}

remove_pairs <- function(vec){
  midpoints <- which(midpoint(vec) == 0)
  mid1 <- midpoints + 1
  index <- which(duplicated(c(midpoints, mid1)) == TRUE)
  repeated_value <- c(midpoints, mid1)[index]
  diff_positions <- midpoints[!midpoints %in% repeated_value]
  positions_to_remove <- c(diff_positions, diff_positions+1)

  new_vec <- vec[!1:length(vec) %in% positions_to_remove]
  new_vec
}


remove_pairs_recursive <- function(vec){
  new_vec <- remove_pairs(vec)
  
  if(length(vec) == length(new_vec)){
    length(new_vec)
  } else {
    remove_pairs_recursive(new_vec)
  }
}

remove_a_digit <- function(vec, digit){
  vec[!vec %in% c(digit, -digit)]
  
}

shortest_polymer <- function(vec){
  
  all_numbers <- unique(abs(vec))
  
  short_polymer <- function(vec, digit){
    vec <- remove_a_digit(vec, digit)
    
    remove_pairs_recursive(vec)
  }
  
  polymer_length <- sapply(all_numbers, short_polymer, vec = vec)
  min(polymer_length)
}

