# Part 1

dat <- read.table("cazza_d01.txt")
val <- sum(dat[1:960, ])


# Part 2

dat_vec <- dat[ ,1]
freq <- cumsum(dat_vec)
val <- tail(freq, n = 1)

df <- func1(values = freq, modulus = val)

unique(df[ , "value_mod"])
which( df[ , "value_mod"] == duplicated(df[ , "value_mod"]))

index_list <- value_locations(df[ , "value_mod"])

thing1 <- sapply(1:length(index_list),
                 function(n){diff( c(df[index_list[[n]][1], "value"], 
                                     df[index_list[[n]][2], "value"]) ) / val})


which(min(abs(thing1), na.rm = TRUE) == thing1)
index_list[[57]]
freq[553]

############
# functions

func1 <- function(values, modulus){
  new_vals <- values %% modulus
  multiplicative_value <- floor(values/modulus)
  index <- seq(1:length(values))
  
  data.frame("index" = index, 
             "value" = values, 
             "value_mod" = new_vals, 
             "multiplicative_value" = multiplicative_value)
}


value_locations <- function( mod_values){
  unique_mod_values <- unique(mod_values)
  
  frequency <- lapply(1:length(unique_mod_values),
                      function(n)  which(mod_values == unique_mod_values[n]))
  frequency
}
