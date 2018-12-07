dat <- readLines("data/d07.txt", n = -1)
dat_list <- strsplit(dat, split = "")

dat_list[[2]][c(6,37)]

letter_list <- lapply(1:length(dat_list), 
       function(n){
        c("first" = dat_list[[n]][6], "second" = dat_list[[n]][37])
  
})

first <- unlist(lapply(letter_list, `[[`, 1))
second <- unlist(lapply(letter_list, `[[`, 2))

mat <- cbind(first, second)
alphabetical_first <- sort(mat[, 1], index.return = TRUE)
ordered_second <- second[alphabetical$ix]
ordered_mat <- cbind(alphabetical_first$x, ordered_second)
ordered_mat
