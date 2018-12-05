# Part 1

dat <- read.table("data/d01_1.txt")
val <- sum(dat[1:960, ])


# Part 2

dat_vec <- dat[ ,1]
freq <- cumsum(dat_vec)
val <- tail(freq, n = 1)

df <- func1(values = freq, modulus = val)

unique(df[ , "new_vals"])
which( df[ , "new_vals"] == duplicated(df[ , "new_vals"]))

index_list <- value_frequency(df[ , "new_vals"])

thing1 <- sapply(1:length(index_list),
                 function(n){diff( c(df[index_list[[n]][1], "values"], 
                                     df[index_list[[n]][2], "values"]) ) / val})


which(min(abs(thing1), na.rm = TRUE) == thing1)
index_list[[57]]
freq[553]


func1 <- function(values, modulus){
  new_vals <- values %% modulus
  multiplicative_value <- floor(values/modulus)
  index <- seq(1:length(values))
  
  data.frame("index" = index, 
             "value" = values, 
             "value_mod" = new_vals, 
             "multiplicative_value" = multiplicative_value)
}


x <- c(0, 1, 2, 3, 1, 2, 1, 4, 6, 7, 6, 3)
y <- diff(x)
value_locations(x, func1(x, 3))
#func2(x, )

func2 <- function(values, modulus){
  new_vals <- values %% modulus
  multiplicative_value <- floor(values/modulus)
  index <- seq(1:length(values))
  
  frequency_index_list <- value_frequency(new_vals)
  
  frequency_index_list

}


value_locations <- function(values, mod_values){
  unique_mod_values <- unique(mod_values)
  
  frequency <- lapply(1:length(unique_mod_values),
                      function(n) rbind("mod_value" = unique_mod_values[n], 
                                        "location" = which(values == unique_mod_values[n]),
                                        "value" = values))
  frequency
}

###################################################
# NOTES

dat_vec <- c(-6, 3, 8, 5, -6)
freq <- cumsum(dat_vec)
val <- tail(freq, n = 1)
freq
freq %% val

floor(freq/val)


freq2 <- freq + tail(freq, n = 1)
freq3 <- freq2 + tail(freq, n = 1)
which(dat_vec==max(dat_vec))

c(freq, freq2, freq3)

asc_freq <- freq[order(freq)]

min(freq)
max(freq)

func1 <- function(values, modulus){
  new_vals <- values %% modulus
  multiplicative_value <- floor(values/modulus)
  index <- seq(1:length(values))
  
  data.frame(index, values, new_vals, multiplicative_value)
}

df <- func1(values = freq, modulus = val)

unique(df[ , "new_vals"])
which( df[ , "new_vals"] == duplicated(df[ , "new_vals"]))

x <- c(1, 2, 3, 1, 2, 1)
duplicated(x[which(x == duplicated(x))])

value_frequency <- function(values){
  unique_values <- unique(values)
  
  frequency <- lapply(1:length(unique_values),
                   function(n) which(values == unique_values[n]))
  frequency

  }

index_list <- value_frequency(df[ , "new_vals"])
index_list[[173]][1]

thing1 <- sapply(1:length(index_list),
       function(n){diff( c(df[index_list[[n]][1], "values"], df[index_list[[n]][2], "values"]) ) / val})
diff( c(df[index_list[[173]][1], "values"], df[index_list[[173]][2], "values"]) )

which(min(abs(thing1), na.rm = TRUE) == thing1)
freq[553]

index_list[[57]]
#################################################


# Part 2
dat_vec <- dat[ , 1]
# dat_vec <- c(3, 3, 4, -2, -4)
# dat_vec <- c(-6, 3, 8, 5, -6)
# dat_vec <- c(7, 7, -2, -7, -4)
# dat_vec <- c(1, -1)


which(dat_vec==max(dat_vec))

freq <- cumsum(dat_vec)

which(freq > 505)


freq_diff <- c(diff(freq), diff(c(tail(freq, n = 1), freq[1])))
val <- tail(freq, n=1)
pos <- which(freq_diff/(val) == floor(freq_diff/(val)))
freq[pos+1]

scaled_freq <- freq / val
which(scaled_freq == floor(scaled_freq))




###############################################################

freq2 <- freq + tail(freq, n = 1)
freq3 <- freq2 + tail(freq, n = 1)
freq4 <- freq3 + tail(freq, n = 1)
freq5 <- freq4 + tail(freq, n = 1)
freq6 <- freq5 + tail(freq, n = 1)

diff(freq)



sum(duplicated(c(0, freq, freq2, freq3)))
which(duplicated(c(0, freq, freq2, freq3)) == c(0, freq, freq2, freq3))


x <- c(1, 2, 3)
y <- c(1, 5, 6)
duplicated(c(x,y))

x <- c(5.3, 5.4, 5, 5.5, 6)
which(x == floor(x))
