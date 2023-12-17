fix_step2 <- function(features, disp_outl, STBL_PRED=7){
  require(sf)
  require(tidyverse)
  require(lubridate)
  leaving <- features[[2]] %>% filter(!is.na(displacement)) %>% mutate(Intv = interval(ymd_hms(start_time_chr)-duration(STBL_PRED,"days"), ymd_hms(end_time_chr)))
  disp_outl <- disp_outl %>% rowwise() %>% mutate(outlier_fixed = any(time %within% leaving$Intv)) %>% mutate(clusterID = case_when(
    outlier_fixed ~ clusterID,
    !outlier_fixed ~ 0
  )) %>% mutate(peak = outlier_fixed)
  return(disp_outl)
}