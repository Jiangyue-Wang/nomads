# requirements for movement track file
# column names: x(numeric format), y(numeric format), Time(lubridate  format)
step1 <- function(mvtrk, window = 1:30, method = "smooth"){
  require(tidyverse)
  require(sf)
  require(lubridate)
  source("src/FUNCTION-location_variance.R")
  
  time_lag_var <- data.frame(time_lag = NA, var = NA)
  
  for(timelag in window){
    start_time <- min(mvtrk$Time)
    end_time <- max(mvtrk$Time)
    
    end_row <- max(which(mvtrk$Time<=(end_time - duration(days=timelag))))
  
    for(j in round(seq(1,end_row, length.out = 30))){
      first_position_time <- mvtrk[j,"Time"]
      
      # filter temporary positions for variance calculation
      tmp_position <- mvtrk %>% filter(Time-duration(days = timelag)<=first_position_time) %>% filter(Time >= first_position_time)
      
      if(nrow(tmp_position)<3){ next }
      
      tmp_var <- location_variance(tmp_position$x, tmp_position$y) %>% as_tibble() %>% mutate(time_lag = timelag) %>% rename(var = value)
      time_lag_var <- bind_rows(time_lag_var,tmp_var)
      rm(tmp_position)
      rm(tmp_var)
      gc()
    }
  }
  # ggplot(time_lag_var, aes(x = time_lag, y = var)) + geom_point() + geom_smooth()
  return(time_lag_var)  
}
