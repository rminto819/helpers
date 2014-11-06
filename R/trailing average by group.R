
library(dplyr)

is.even <- function(x) {
  x %% 2 == 0
}

data <- mutate(data, trail_index = NA) 



### create index for the trailing average to reference

for(i in 2:nrow(data)) {
  data$trail_index[i] <- ifelse(data$id[i] == data$id[i - 1], data$trail_index[i-1] + 1, 1) 
}


data <- mutate(data, trail_avg = NA)
data <- mutate(data, trail_med = NA)


### create the trailing average and median for x

for(i in 2:nrow(data)) {
  j <- i - data$trail_index[i] + 1
  k <- i
  
  data$trail_avg[i] <- rollmean(data$x[j:k], data$trail_index[i], align = 'right')
  data$trail_med[i] <- rollmedian(data$x[j:k], ifelse(is.even(data$trail_index[i]), data$trail_index[i] - 1, data$trail_index[i]), align = 'right')
  
}




















