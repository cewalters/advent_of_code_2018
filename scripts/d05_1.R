# dat <- readLines("data/d05_1.txt", n = -1L)
# dat_char_vec <- unlist(strsplit(dat, split = ""))
# dat_vec <- unname(vapply(dat_char_vec, letter2integer, numeric(1)))
# save(dat_vec, file = "data/d05_1_numeric.Rds")

# read in dat_vec
load(file = "data/d05_1_numeric.Rds")

polymer_length(dat_vec[1:30])

tvec <- dat_vec[1:35]
polymer_length(tvec)

sum(midpoint(tvec) ==0)
subs <- create_subvectors(tvec)
odd_palindrome_indices(subs[[3]][ , 2], subs[[3]][ , 1])

##################################################

subvecs <- create_subvectors(dat_vec)
subvecs[[32]]

odd_palindrome_indices(subvecs[[32]][ , 2], 
                       subvecs[[32]][ , 1])





polymer_reduction <- function(num_vec){
  
  sub_vecs <- create_subvectors(num_vec)
  palindrome_indices <- lapply(1:length(sub_vecs), 
                               function(n) odd_palindrome_indices(vec = sub_vecs[[n]][ , 2], 
                                                                  vec_index = sub_vecs[[n]][ , 1]))
  
  repeated <- repeats(palindrome_indices)
  to_remove
  
  # some values may be in 2 palindromes hence counted twice.
  # need to not double count any.
  number_reacted_values <- calc_input_palindrome_lengths(palindrome_indices) - calc_repeats(palindrome_indices)
  length(num_vec) - number_reacted_values
}


repeats <- function(palindrome_indices_list){
  # only interested in the edge values of palindromes.  If consecutive edge values 
  #  appear, there share a value in our input vec.
  
    edges <- lapply(1:3, function(n) c(min(palindrome_indices_list[[n]]), 
                                       max(palindrome_indices_list[[n]]) )) 
    unique_edges <- unlist(lapply(edges, unique))
    
    # duplicated_edges
    index <- which(duplicated(c(unique_edges, unique_edges+1)) == TRUE)
    c(unique_edges, unique_edges+1)[index]
  
}
# It is all wrong :(

###########################################

length(unique(dat_vec))
value_frequency(dat_vec)

mid <- midpoint(dat_vec)
value_frequency(mid)
sum(match(mid, 0), na.rm = TRUE)
# each value appears the same number of times as it's negative (I think, just looked at data)

# find the 0 midpoint values.  Take the cumulative sum each direction from the 0. Provided at 
#  every step away the LHS = -RHS then that forms part of a chain.
# Do for all zeros.  emove all the values that this corresponds to.
# Can chains ever "crash" into each other? ie try to use the same value in both?

###################################################
# functions

letter2integer <- function(character_letter){
  df <- data.frame("letters" = c(letters, LETTERS), 
                   "value" = c(seq(1:26), seq(-1, -26)))
  df[ which(df$letters == character_letter), "value" ]
}


value_frequency <- function(values){
  unique_values <- unique(values)
  
  frequency <- sapply(1:length(unique_values),
                      function(n)  sum(values == unique_values[n]))
  cbind(unique_values, frequency)
}


midpoint <- function(vec){
  vec[-length(vec)] + diff(vec)/2
}


min_distance_between_zeros <- function(midpoint_vector){
  zeros <- which(c(0, midpoint_vector, 0) == 0)
  diff_zeros <- diff(zeros) - 1
  mat <- cbind(diff_zeros[ -length(diff_zeros)], diff_zeros[-1])
  min_distances <- apply(mat, 1, min)
  
  cbind("index" = which(midpoint_vector == 0), "min_dist" = min_distances)
}


create_subvectors <- function(vec){
  
  midpoints <- midpoint(vec)
  mat <- min_distance_between_zeros(midpoints)
  
  calc_indices <- function(index, min_dist){
    seq(index - min_dist, index + min_dist)
  }
  
  subvec_indices <- lapply(1:nrow(mat), 
                           function(n) calc_indices(index = mat[ n, "index"], 
                                                    min_dist = mat[ n, "min_dist"]) )
  
  diffs <- diff(vec)
  lapply(1:length(subvec_indices), 
         function(n) cbind("index" = subvec_indices[[n]], 
                           "diffs" = diffs[subvec_indices[[n]]])) 
}


odd_palindrome_length <- function(vec){
  middle_position <- ceiling(length(vec)/2)
  Rvec <- vec[(middle_position + 1):length(vec)]
  Lvec <- rev(vec[(1:middle_position - 1)])
  
  # find first position where the vectors DO NOT match
  no_match_index <- min(which(Rvec != Lvec))
  palindrome_length <- 1 + (no_match_index-1)*2
  palindrome_length
}


odd_palindrome_indices <- function(vec, vec_index){
  if(length(vec) != length(vec_index)){
    message("vector and corresponding indices are incorrect.  You have clearly 
            done something terrible. Raaah. Terrible lizard.")
  } else if(length(vec) == 1) {
    unname(vec_index)
  } else {
    
    middle_position <- ceiling(length(vec)/2)
    middle_index <- vec_index[middle_position]
    Rvec <- vec[(middle_position + 1):length(vec)]
    Lvec <- rev(vec[(1:middle_position - 1)])
    
    # find first position where the vectors DO NOT match, take one off to get end
    #  of palindrome chain
    equal <- Rvec == Lvec
    if(FALSE %in% equal){
      final_match_length <- min(which(Rvec != Lvec)) - 1
    } else {
      final_match_length <- length(Rvec)
    }
    
    match_indices <- seq(middle_index - final_match_length, 
                         middle_index + final_match_length)
    
    #print(match_indices)
    match_indices
  }
}

calc_repeats <- function(palindrome_indices_list){
  # only interested in the edge values of palindromes.  If consecutive edge values 
  #  appear, there share a value in our input vec.
  
  number_repeated_edges <- function(palindrome_indices_list){
    edges <- lapply(1:3, function(n) c(min(palindrome_indices_list[[n]]), 
                                       max(palindrome_indices_list[[n]]) )) 
    unique_edges <- unlist(lapply(edges, unique))
    
    #sum the number of consecutive pairs of edges
    sum(duplicated(c(unique_edges, unique_edges+1)))
  }
  
  2*number_repeated_edges(palindrome_indices_list)
  
}

polymer_length <- function(num_vec){
  
  sub_vecs <- create_subvectors(num_vec)
  palindrome_indices <- lapply(1:length(sub_vecs), 
                               function(n) odd_palindrome_indices(vec = sub_vecs[[n]][ , 2], 
                                                                  vec_index = sub_vecs[[n]][ , 1]))
  
  print(palindrome_indices)
  
  # calc the palindrome lengths in input vector.  We can then subtract the sum
  #  of these lengths from length of the input vector to get our answer
  calc_input_palindrome_lengths <- function(palindrome_indices_list){
    
    each_length <- lapply(palindrome_indices_list, function(...) length(...) + 1)
    sum(unlist(each_length))
  }
  
  # some values may be in 2 palindromes hence counted twice.
  # need to not double count any.
  number_reacted_values <- calc_input_palindrome_lengths(palindrome_indices) - calc_repeats(palindrome_indices)
  length(num_vec) - number_reacted_values
  }


##################################################
# example: solution is 10 units
ex <- "dabAcCaCBAcCcaDA"
ex_char <- unlist(strsplit(ex, split = ""))
ex_vec <- unname(vapply(ex_char, letter2integer, numeric(1)))

calc_repeats(ex_vec)
polymer_length(ex_vec)

subs <- create_subvectors(ex_vec)
pals <- lapply(1:length(subs), 
               function(n) odd_palindrome_indices(vec = subs[[n]][ , 2], 
                                                  vec_index = subs[[n]][ , 1]))

repeats(pals)
subs
ex_vec
