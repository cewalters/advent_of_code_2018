dat <- read.table("data/d05_1.txt", stringsAsFactors = FALSE)
dat_string <- dat[ , 1]
dat_vec <- unlist(strsplit(dat_string, split = ""))
alphabet <- letters


ex <- unlist(strsplit("dabAcCaCBAcCcaDA", split = ""))
pos_numbers <- seq(1:26)
neg_numbers <- -1 * pos_numbers

#  just replace letters with their index??
