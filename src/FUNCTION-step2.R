# requirements for movement track file
# column names: x(numeric format), y(numeric format), Time(lubridate  format)
step2 <- function(mvtrk, STBL_PRED = 7){
  require(sf)
  require(tidyverse)
  require(lubridate)
  source("src/FUNCTION-location_variance.R")
  # calculate oscillation figure values
  mvtrk <- mvtrk %>% arrange(Time)
  END_T <-  max(mvtrk$Time)
  END_LOOP_T <- END_T - duration(STBL_PRED, "days")
  END_LOOP <- mvtrk %>% filter(Time <= END_LOOP_T) %>% nrow()

  continuous_var <- data.frame()
  
  for(i in 1:END_LOOP){
    start_time <- mvtrk$Time[i]
    tmp_locs <- mvtrk %>% filter(Time<=(start_time+duration(STBL_PRED, "days"))) %>% filter(Time>=start_time)
    
    tmp_area <- location_variance(tmp_locs$x, tmp_locs$y) %>% as_tibble() %>% mutate(time = mvtrk$Time[i]) %>% rename(Var = `value`)
    continuous_var <- bind_rows(continuous_var, tmp_area)
    
    rm(tmp_locs)
    rm(tmp_area)
    gc()
  }
  # ggplot(continuous_var, aes(x = time, y = Var)) + geom_point()
  # identify peaks
  disp_var_outl <- continuous_var %>% mutate(rownum = row_number()) %>% arrange(desc(Var)) %>% mutate(outlier = F)
  
  for(i in 1:nrow(disp_var_outl)){
    mean_var <- disp_var_outl %>% filter(!outlier) %>% dplyr::select(Var) %>% as.vector() %>% unlist() %>% mean()
    std_var <- disp_var_outl %>% filter(!outlier) %>% dplyr::select(Var) %>% as.vector() %>% unlist() %>% sd()
    if(disp_var_outl$Var[i]>(mean_var+3*std_var)){
      disp_var_outl$outlier[i] <- T
    }
    if(!disp_var_outl$outlier[i]) {break}
  }
   # ggplot(disp_var_outl, aes(x = time, y = Var, color = outlier)) + geom_point(alpha = 0.5, size = 0.5) + theme_bw() + xlab("Time") + ylab("Variance (km^2)")
  if (all(!disp_var_outl$outlier)) {
    cat("All locations are non-outliers.")
    return(disp_var_outl)
  }
  # assign cluster ID
  tmp_outl <- disp_var_outl %>% arrange(time) %>% filter(outlier) %>% mutate(clusterID = NA)
  clus_id <- 1
  
  for(i in 2:nrow(tmp_outl)){
    if((tmp_outl$rownum[i]-tmp_outl$rownum[i-1])>1){
      clus_id <- clus_id+1
    }
    tmp_outl$clusterID[i] <- clus_id
  }
  disp_var_outl <- left_join(disp_var_outl, tmp_outl[,c("rownum", "clusterID")], by = "rownum")
  
  # complete peak
  disp_outl <- disp_var_outl %>% arrange(time)
  disp_outl$clusterID[is.na(disp_outl$clusterID)] <- 0
  
  last_peak_end <- disp_outl$time[1]
  for(i in 1:length(unique(disp_outl$clusterID[disp_outl$clusterID!=0]))){
    peak_start <- min(disp_outl$time[disp_outl$clusterID==i])
    tmp_loc <- disp_outl %>% filter(time<=peak_start, time>=last_peak_end)
    if(nrow(tmp_loc)<2){next}
    for(j in nrow(tmp_loc):2){
      if((tmp_loc$Var[j]-tmp_loc$Var[j-1])>=0){
        disp_outl$clusterID[disp_outl$rownum==tmp_loc$rownum[j]] <- i
      }
      else{break}
    }
    peak_end <- max(disp_outl$time[disp_outl$clusterID==i])
    tmp_loc <- disp_outl %>% filter(time>=peak_end)
    
    for(j in 2:nrow(tmp_loc)){
      if(nrow(tmp_loc) < 2){break}
      if((tmp_loc$Var[j]-tmp_loc$Var[j-1])<=0){
        disp_outl$clusterID[disp_outl$rownum==tmp_loc$rownum[j]] <- i
      }
      else{break}
    }
    last_peak_end <-max(disp_outl$time[disp_outl$clusterID==i])
  }
  disp_outl <- disp_outl %>% mutate(peak = clusterID!=0)
  # ggplot(disp_outl, aes(x = time, y = Var, color = as.factor(clusterID))) + geom_point(alpha = 0.5, size = 0.5) + theme_bw() + xlab("Time") + ylab("Variance")
  return(disp_outl)
}