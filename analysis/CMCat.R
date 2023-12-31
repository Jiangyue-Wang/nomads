# load library----
library(tidyverse)
library(sf)
library(terra)
library(lubridate)
source("src/FUNCTION-location_variance.R")
source("src/FUNCTION-step1.R")
source("src/FUNCTION-step2.R")
source("src/FUNCTION-step3.R")
source("src/FUNCTION-fix_step2.R")

# clean data----
# ind 4,6,7,8 are sedentary, 1,2,3,5,9 are nomadic
cmcat1 <- read.csv("data/CMCat/cleaned/PKU001_new.csv") %>% dplyr::select(x_, y_, t_) %>% rename(x = x_, y = y_, Time = t_) %>% mutate(Time = ymd_hms(Time) ) %>% arrange(Time)
cmcat2 <- read.csv("data/CMCat/cleaned/PKU002_new.csv") %>% dplyr::select(x_, y_, t_) %>% rename(x = x_, y = y_, Time = t_) %>% mutate(Time = ymd_hms(Time) ) %>% arrange(Time)
cmcat3 <- read.csv("data/CMCat/cleaned/PKU003_new.csv") %>% dplyr::select(x_, y_, t_) %>% rename(x = x_, y = y_, Time = t_) %>% mutate(Time = ymd_hms(Time) ) %>% arrange(Time)
cmcat4 <- read.csv("data/CMCat/cleaned/PKU004_new.csv") %>% dplyr::select(x_, y_, t_) %>% rename(x = x_, y = y_, Time = t_) %>% mutate(Time = ymd_hms(Time) ) %>% arrange(Time)
cmcat5 <- read.csv("data/CMCat/cleaned/PKU005_new.csv") %>% dplyr::select(x_, y_, t_) %>% rename(x = x_, y = y_, Time = t_) %>% mutate(Time = ymd_hms(Time) ) %>% arrange(Time)
cmcat6 <- read.csv("data/CMCat/cleaned/PKU006_new.csv") %>% dplyr::select(x_, y_, t_) %>% rename(x = x_, y = y_, Time = t_) %>% mutate(Time = ymd_hms(Time) ) %>% arrange(Time)
cmcat7 <- read.csv("data/CMCat/cleaned/PKU007_new.csv") %>% dplyr::select(x_, y_, t_) %>% rename(x = x_, y = y_, Time = t_) %>% mutate(Time = ymd_hms(Time) ) %>% arrange(Time)
cmcat8 <- read.csv("data/CMCat/cleaned/PKU008_new.csv") %>% dplyr::select(x_, y_, t_) %>% rename(x = x_, y = y_, Time = t_) %>% mutate(Time = ymd_hms(Time) ) %>% arrange(Time)
cmcat9 <- read.csv("data/CMCat/cleaned/PKU009_new.csv") %>% dplyr::select(x_, y_, t_) %>% rename(x = x_, y = y_, Time = t_) %>% mutate(Time = ymd_hms(Time) ) %>% arrange(Time)
cmcat10 <- read.csv("data/CMCat/cleaned/PKU010_new.csv") %>% dplyr::select(x_, y_, t_) %>% rename(x = x_, y = y_, Time = t_) %>% mutate(Time = ymd_hms(Time) ) %>% arrange(Time)

lags4 <- step1(mvtrk = cmcat4)
ggplot(lags4, aes(x = time_lag, y = var)) + geom_point() + geom_smooth()
saveRDS(lags4, "intermediate_rds/CMCat_lags4.rds")
ggsave("output/CMCat_lags4.png", width = 6, height = 4)
lags6 <- step1(mvtrk = cmcat6)
ggplot(lags6, aes(x = time_lag, y = var)) + geom_point() + geom_smooth()
saveRDS(lags6, "intermediate_rds/CMCat_lags6.rds")
ggsave("output/CMCat_lags6.png", width = 6, height = 4)
lags7 <- step1(mvtrk = cmcat7)
ggplot(lags7, aes(x = time_lag, y = var)) + geom_point() + geom_smooth()
saveRDS(lags7, "intermediate_rds/CMCat_lags7.rds")
ggsave("output/CMCat_lags7.png", width = 6, height = 4)
lags8 <- step1(mvtrk = cmcat8)
ggplot(lags8, aes(x = time_lag, y = var)) + geom_point() + geom_smooth()
saveRDS(lags8, "intermediate_rds/CMCat_lags8.rds")
ggsave("output/CMCat_lags8.png", width = 6, height = 4)
lags10 <- step1(mvtrk = cmcat10)
ggplot(lags10, aes(x = time_lag, y = var)) + geom_point() + geom_smooth()
saveRDS(lags10, "intermediate_rds/CMCat_lags10.rds")
ggsave("output/CMCat_lags10.png", width = 6, height = 4)
# 10 days will be fine
outl5 <- step2(mvtrk = cmcat5, STBL_PRED = 10)
ggplot(outl5, aes(x = time, y = Var, color = as.factor(outlier))) + geom_point(alpha = 0.5, size = 0.5) + theme_bw() + xlab("Time") + ylab("Variance")
ggsave("output/CMCat_outl5.png", width = 6, height = 4)
features5 <- step3(mvtrk = cmcat5, disp_outl = outl5, STBL_PRED = 10)
# The peak identification doesn't work when residence time nearly equals to non-residence time



outl1 <- step2(mvtrk = cmcat1, STBL_PRED = 10)
outl2 <- step2(mvtrk = cmcat2, STBL_PRED = 10)
outl3 <- step2(mvtrk = cmcat3, STBL_PRED = 10)
outl5 <- step2(mvtrk = cmcat5, STBL_PRED = 10)
outl9 <- step2(mvtrk = cmcat9, STBL_PRED = 10)
saveRDS(outl1, "intermediate_rds/CMCat_outl1.rds")
saveRDS(outl2, "intermediate_rds/CMCat_outl2.rds")
saveRDS(outl3, "intermediate_rds/CMCat_outl3.rds")
saveRDS(outl5, "intermediate_rds/CMCat_outl5.rds")
saveRDS(outl9, "intermediate_rds/CMCat_outl9.rds")


features1 <- step3(mvtrk = cmcat1, disp_outl = outl1, STBL_PRED = 10)
features2 <- step3(mvtrk = cmcat2, disp_outl = outl2, STBL_PRED = 10)
features3 <- step3(mvtrk = cmcat3, disp_outl = outl3, STBL_PRED = 10)
features9 <- step3(mvtrk = cmcat9, disp_outl = outl9, STBL_PRED = 10)


outl_fixed1 <- fix_step2(features = features1, disp_outl = outl1,STBL_PRED = 10)
saveRDS(outl_fixed1,"intermediate_rds/CMCat_outl_fixed1.rds")
ggplot(outl_fixed1, aes(x = time, y = Var, color = as.factor(outlier_fixed))) + geom_point(alpha = 0.5, size = 0.5) + theme_bw() + xlab("Time") + ylab("Variance")
ggsave("output/CMCat_outl1_fixed.png", width = 6, height = 4)
features_fixed1 <-step3(mvtrk = cmcat1, disp_outl = outl_fixed1, STBL_PRED = 10)
write.csv(features_fixed1[[1]],"output/cmcat1_R.csv")
write.csv(features_fixed1[[2]],"output/cmcat1_N.csv")

outl_fixed2 <- fix_step2(features = features2, disp_outl = outl2,STBL_PRED = 10)
saveRDS(outl_fixed2,"intermediate_rds/CMCat_outl_fixed2.rds")
ggplot(outl_fixed2, aes(x = time, y = Var, color = as.factor(outlier_fixed))) + geom_point(alpha = 0.5, size = 0.5) + theme_bw() + xlab("Time") + ylab("Variance")
ggsave("output/CMCat_outl2_fixed.png", width = 6, height = 4)
features_fixed2 <-step3(mvtrk = cmcat2, disp_outl = outl_fixed2, STBL_PRED = 10)
write.csv(features_fixed2[[1]],"output/cmcat2_R.csv")
write.csv(features_fixed2[[2]],"output/cmcat2_N.csv")

outl_fixed3 <- fix_step2(features = features3, disp_outl = outl3,STBL_PRED = 10)
saveRDS(outl_fixed3,"intermediate_rds/CMCat_outl_fixed3.rds")
ggplot(outl_fixed3, aes(x = time, y = Var, color = as.factor(outlier_fixed))) + geom_point(alpha = 0.5, size = 0.5) + theme_bw() + xlab("Time") + ylab("Variance")
ggsave("output/CMCat_outl3_fixed.png", width = 6, height = 4)
features_fixed3 <-step3(mvtrk = cmcat3, disp_outl = outl_fixed3, STBL_PRED = 10)
write.csv(features_fixed3[[1]],"output/cmcat3_R.csv")
write.csv(features_fixed3[[2]],"output/cmcat3_N.csv")

outl_fixed9 <- fix_step2(features = features9, disp_outl = outl9,STBL_PRED = 10)
saveRDS(outl_fixed9,"intermediate_rds/CMCat_outl_fixed9.rds")
ggplot(outl_fixed9, aes(x = time, y = Var, color = as.factor(outlier_fixed))) + geom_point(alpha = 0.5, size = 0.5) + theme_bw() + xlab("Time") + ylab("Variance")
ggsave("output/CMCat_outl9_fixed.png", width = 6, height = 4)
features_fixed9 <-step3(mvtrk = cmcat9, disp_outl = outl_fixed9, STBL_PRED = 10)
write.csv(features_fixed9[[1]],"output/cmcat9_R.csv")
write.csv(features_fixed9[[2]],"output/cmcat9_N.csv")

# try 4 6 8 10

outl4 <- step2(mvtrk = cmcat4, STBL_PRED = 10)
features4 <- step3(mvtrk = cmcat4, STBL_PRED = 10, disp_outl = outl4)
outl_fixed4 <- fix_step2(features = features4, disp_outl = outl4, STBL_PRED = 10)
saveRDS(outl_fixed4,"intermediate_rds/CMCat_outl_fixed4.rds")
ggplot(outl_fixed4, aes(x = time, y = Var, color = as.factor(outlier_fixed))) + geom_point(alpha = 0.5, size = 0.5) + theme_bw() + xlab("Time") + ylab("Variance")
ggsave("output/CMCat_outl4_fixed.png", width = 6, height = 4)
features_fixed4 <- step3(mvtrk = cmcat4, disp_outl = outl_fixed4, STBL_PRED = 10)
write.csv(features_fixed4[[1]],"output/cmcat4_R.csv")
write.csv(features_fixed4[[2]],"output/cmcat4_N.csv")
# need to manually fix the non-resident feature, because signal lost more than two month and cause some troubles

outl6 <- step2(mvtrk = cmcat6, STBL_PRED = 10) # all non-outliers
saveRDS(outl6,"intermediate_rds/CMCat_outl6.rds")
ggplot(outl6, aes(x = time, y = Var, color = as.factor(outlier))) + geom_point(alpha = 0.5, size = 0.5) + theme_bw() + xlab("Time") + ylab("Variance")
ggsave("output/CMCat_outl6.png", width = 6, height = 4)



outl7 <- step2(mvtrk = cmcat7, STBL_PRED = 10)
features7 <- step3(mvtrk = cmcat7, disp_outl = outl7, STBL_PRED = 10)
outl_fixed7 <- fix_step2(features = features7, disp_outl = outl7, STBL_PRED = 10)
saveRDS(outl_fixed7,"intermediate_rds/CMCat_outl_fixed7.rds")
ggplot(outl_fixed7, aes(x = time, y = Var, color = as.factor(outlier_fixed))) + geom_point(alpha = 0.5, size = 0.5) + theme_bw() + xlab("Time") + ylab("Variance")
ggsave("output/CMCat_outl7_fixed.png", width = 6, height = 4)
features_fixed7 <- step3(mvtrk = cmcat7, disp_outl = outl_fixed7, STBL_PRED = 10)
write.csv(features_fixed7[[1]],"output/cmcat7_R.csv")
write.csv(features_fixed7[[2]],"output/cmcat7_N.csv")


outl8 <- step2(mvtrk = cmcat8, STBL_PRED = 10)
features8 <- step3(mvtrk = cmcat8, disp_outl = outl8, STBL_PRED = 10)
outl_fixed8 <- fix_step2(features = features8, disp_outl = outl8, STBL_PRED = 10)
saveRDS(outl_fixed8,"intermediate_rds/CMCat_outl_fixed8.rds")
ggplot(outl_fixed8, aes(x = time, y = Var, color = as.factor(outlier_fixed))) + geom_point(alpha = 0.5, size = 0.5) + theme_bw() + xlab("Time") + ylab("Variance")
ggsave("output/CMCat_outl8_fixed.png", width = 6, height = 4)
features_fixed8 <- step3(mvtrk = cmcat8, disp_outl = outl_fixed8, STBL_PRED = 10)
write.csv(features_fixed8[[1]],"output/cmcat8_R.csv")
write.csv(features_fixed8[[2]],"output/cmcat8_N.csv")

outl10 <- step2(mvtrk = cmcat10, STBL_PRED = 10) # all non-outliers
saveRDS(outl10,"intermediate_rds/CMCat_outl10.rds")
ggplot(outl10, aes(x = time, y = Var, color = as.factor(outlier))) + geom_point(alpha = 0.5, size = 0.5) + theme_bw() + xlab("Time") + ylab("Variance")
ggsave("output/CMCat_outl10.png", width = 6, height = 4)
