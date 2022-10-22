library(stringr)
library(ggplot2)
library(ggsci)
library(tidyverse)
library(lubridate)
#install.packages("gridExtra")
library(gridExtra)



list_pt_ABO <- c("PT_A", "PT_B", "PT_O", "PT_AB")
list_gt_ABO <- c("GT_AA", "GT_AB", "GT_AO", "GT_BB", "GT_BO", "GT_OO")

# Simulation 
func_sim_iteration <- function(percent, Tsim = 250, Nsim=1000){
  df_sim_gt <- data.frame(matrix(percent, nrow=1))
  colnames(df_sim_gt) <- list_gt_ABO
  
  df_sim_pt <- data.frame(matrix(list_pt_ABO, nrow=1))[numeric(0), ]
  colnames(df_sim_pt) <- list_pt_ABO
  
  df_sim_pt[1,]<-func_gt2pt(df_sim_gt[1,])
  
  for(tsim in 2:Tsim){
    lis_prop_iter_sim<-prop.table(table(func_pop_iteration(df_sim_gt[tsim-1,], Nsim = Nsim)))
    nm_lis<-names(lis_prop_iter_sim)
    df_sim_gt[tsim,]<-0
    for(nlis in 1:length(nm_lis)){
      df_sim_gt[tsim, nm_lis[nlis]]<-lis_prop_iter_sim[nlis]
    }
    df_sim_pt[tsim,]<-func_gt2pt(df_sim_gt[tsim,])
  }
  return(list(df_sim_gt=df_sim_gt, df_sim_pt=df_sim_pt))
}

# GTをPTに変換
func_gt2pt <- function(df_gt){
  df_pt <- data.frame(matrix(c(df_gt["GT_AA"]+df_gt["GT_AO"], 
                               df_gt["GT_BB"]+df_gt["GT_BO"], 
                               df_gt["GT_OO"], 
                               df_gt["GT_AB"]), nrow=1))
  colnames(df_pt) <- list_pt_ABO
  return(df_pt)
}

# 次世代GT生成
func_pop_iteration <- function(percent, Nsim=100){
  mat_sample <- matrix(sample(list_gt_ABO, Nsim*2, replace = TRUE, prob = percent), ncol = 2)
  list_iteration <- numeric(Nsim)
  for(i in 1:Nsim){
    list_iteration[i] <- func_iteration(mat_sample[i,])
  }
  return(list_iteration)
}

# 次世代GT生成(個々)
func_iteration <- function(lis){
  lis_split <- strsplit(substr(lis, 4,5), "")
  iterated <- sort(c(sample(lis_split[[1]], 1), sample(lis_split[[2]], 1)))
  return(paste("GT_", iterated[1], iterated[2], sep = ""))
}
