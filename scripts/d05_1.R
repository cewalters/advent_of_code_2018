# dat <- readLines("data/d05_1.txt", n = -1L)
# dat_char_vec <- unlist(strsplit(dat, split = ""))
# dat_vec <- unname(vapply(dat_char_vec, letter2integer, numeric(1)))
# save(dat_vec, file = "data/d05_1_numeric.Rds")

# read in dat_vec
load(file = "data/d05_1_numeric.Rds")

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
  
  calc_indices <- function(index, min_dist){
    seq(index - min_dist, index + min_dist)
  }
  
  subvec_indices <- lapply(1:nrow(mat), 
                           function(n) calc_indices(index = mat[ n, "index"], 
                                                    min_dist = mat[ n, "min_dist"]) )
  
  diffs <- diff(vec)
  lapply(1:length(subvec_indices), function(n) diffs[subvec_indices[[n]]]) 
}
##################################################
# example: solution is 10 units
ex <- "dabAcCaCBAcCcaDA"
ex_char <- unlist(strsplit(ex, split = ""))
ex_vec <- unname(vapply(ex_char, letter2integer, numeric(1)))

#filter(ex_vec, c(0.5, 0.5))

mat <- min_distance_between_zeros(midpoint(ex_vec))

create_subvectors(ex_vec)

# now write a function to identify palindromes




########################################################
vec_diff <- diff(ex_vec)
bound <- floor(length(vec_diff/2))



length(ex_vec)
sum(ex_vec)
unique(ex_vec)
unique(abs(ex_vec))

value_frequency(ex_vec)
midpoint(ex_vec)
which(0 == midpoint(ex_vec))

x <- c(0,0,1)
match(midpoint(vec), 0)

vec <- ex_vec
match(midpoint(vec), 0)
to_remove <- which(midpoint(ex_vec) == 0)
vec <- ex_vec[to_remove*-1]





# str <- paste(sample(c(LETTERS, letters), 80, TRUE), collapse = "")
# x <- strsplit(str, NULL)[[1]]
# tab <- c(rev(LETTERS), "", letters)
# match(x, tab) - 26

rbind(ex_vec, rev(ex_vec))

################################################
