step3 <- function(mvtrk, disp_outl, STBL_PRED = 7){
  require(sf)
  require(tidyverse)
  require(adehabitatHR)
  require(lubridate)
  
  # assign phase ID
  tmp_outl <- disp_outl %>% filter(clusterID == 0) %>% arrange(rownum) %>%mutate(phaseID = NA)
  tmpID <- 1
  for( i in 1:(nrow(tmp_outl)-1)){
    tmp_outl$phaseID[i] <- paste0("R",tmpID)
    if(tmp_outl$rownum[i+1]-tmp_outl$rownum[i]>1){
      tmpID <- tmpID + 1
    }
  }
  tmp_outl$phaseID[nrow(tmp_outl)] <- paste0("R",tmpID)
  disp_outl <- left_join(disp_outl, tmp_outl[,c("rownum","phaseID")], by = "rownum")
  disp_outl[is.na(disp_outl$phaseID),"phaseID"] <- paste0("N", disp_outl$clusterID[is.na(disp_outl$phaseID)])
  
  # features for each residence phase
  res_feature <- data.frame(phaseID = unique(disp_outl$phaseID[!disp_outl$peak]), Period = NA, start_time = NA, end_time = NA, patch_size = NA)
  for(i in 1:nrow(res_feature)){
    tmp_outl <- disp_outl %>% filter(phaseID == res_feature$phaseID[i])
  
    res_feature$start_time[i] <- min(tmp_outl$time)
    res_feature$end_time[i] <- max(tmp_outl$time) + duration(STBL_PRED,"days")
    res_feature$Period[i] <- res_feature$end_time[i] - res_feature$start_time[i]
    tmp_loc <- mvtrk %>% filter(Time <= res_feature$end_time[i] & Time >= res_feature$start_time[i]) %>% st_as_sf(coords = c("x","y")) %>% dplyr::select(geometry) %>% as_Spatial()
    res_feature$patch_size[i] <- kernel.area(kernelUD(tmp_loc), percent = 95)
    
  }
  res_feature$start_time_chr <- as.character(as_datetime(res_feature$start_time))
  res_feature$end_time_chr <- as.character(as_datetime(res_feature$end_time))
  res_feature$Period <- as.duration(res_feature$Period)
  
  # features for each non-residence phase
  nres_feature <- data.frame(phaseID = unique(disp_outl$phaseID[disp_outl$peak]), Period = NA, start_time = NA, end_time = NA, displacement = NA, distance = NA)
  for(i in 1:nrow(nres_feature)){
    tmp_outl <- disp_outl %>% filter(phaseID == nres_feature$phaseID[i])
    
    nres_feature$start_time[i] <- min(tmp_outl$time) + duration(STBL_PRED,"days")
    nres_feature$end_time[i] <- max(tmp_outl$time) 
    nres_feature$Period[i] <- nres_feature$end_time[i] - nres_feature$start_time[i]
    if(nres_feature$Period[i] <= 0){next}
    tmp_loc <- mvtrk %>% filter(Time <= nres_feature$end_time[i] & Time >= nres_feature$start_time[i]) %>% st_as_sf(coords = c("x","y")) %>% mutate(step = NA)
    if(nrow(tmp_loc)<2){next}
    nres_feature$displacement[i] <- st_distance(tmp_loc[tmp_loc$Time==min(tmp_loc$Time),],tmp_loc[tmp_loc$Time==max(tmp_loc$Time),])
    for(j in 2: nrow(tmp_loc)){
      tmp_loc$step[j] <- st_distance(tmp_loc[j-1,],tmp_loc[j,])
    }
    nres_feature$distance[i] <- sum(tmp_loc$step, na.rm = T)
  }
  nres_feature$start_time_chr <- as.character(as_datetime(nres_feature$start_time))
  nres_feature$end_time_chr <- as.character(as_datetime(nres_feature$end_time))
  nres_feature$Period <- as.duration(nres_feature$Period)
  
  phase_feature <- list(res_feature, nres_feature)
  return(phase_feature)
}