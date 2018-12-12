grid_serial_number <- 18 #5177
xmax <- 300
ymax <- 300

subgrid_sum(xmax = xmax, ymax = ymax, grid_serial_number = grid_serial_number)

powers <- fuel_cells(xmax, ymax, 39)
powers[217, 196]
#######################################################################
# functions

fuel_cells <- function(xmax, ymax, grid_serial_number){
  xvec <- seq(1, xmax)
  yvec <- seq(1, ymax)
  
  initial_grid <- vapply(xvec, 
                         function(x){ vapply(yvec, function(y) (x+10)*(grid_serial_number + y*(x+10)), 
                                             numeric(1)) 
                           },
                         numeric(length(yvec))
                         )
  
  huns <- hundredth_digit(initial_grid)
  huns - 5
}


hundredth_digit <- function(value){
  hun <- floor(value/100)
  rounded_hun <- floor(value/1000)*10
  hun - rounded_hun
}

subgrid_sum <- function(xmax, ymax, grid_serial_number){
  
  power_levels <- fuel_cells(xmax, ymax, grid_serial_number)
  
  sol_mat <- matrix(nrow = ymax - 2, ncol = xmax - 2)
  
  for(i in 1:nrow(sol_mat)){
    for(j in 1:ncol(sol_mat)){
      sol_mat[i, j] <- sum( power_levels[ i:(i+2), j:(j+2)] )
    }
  }
  
  list(max(sol_mat), which(sol_mat == max(sol_mat), arr.ind = TRUE))
}
