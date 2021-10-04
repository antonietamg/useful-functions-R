####################################################################
###################                #################################
################### RANDOM SHUFFLE #################################
###################                #################################
####################################################################
####################################################################

#### Preserves the amplitude distribution, the mean and the variance of the original data.

random_shuffle <- function(x){
  wnoise <- arima.sim(model = list(order = c(0, 0, 0)), n = length(x))
  wnoise_df <- data.frame(x, wnoise)
  sorting <- wnoise_df[
    with(wnoise_df, order(wnoise)),
    ]
  sorting <- sorting[,1]
  return(sorting)
}

