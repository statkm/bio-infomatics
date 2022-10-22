library(stringr)
library(ggplot2)
library(ggsci)
library(tidyverse)
library(lubridate)
#install.packages("gridExtra")
library(gridExtra)



list_pt_Aa <- c("PT_A", "PT_a")
list_gt_Aa <- c("GT_AA", "GT_Aa", "GT_aa")



# Simulation 
func_sim_iteration_Aa <- function(percent, Tsim = 250){
  df_sim_gt <- data.frame(matrix(percent, nrow=1))
  colnames(df_sim_gt) <- list_gt_Aa
  
  df_sim_pt <- data.frame(matrix(list_pt_Aa, nrow=1))[numeric(0), ]
  colnames(df_sim_pt) <- list_pt_Aa
  
  df_sim_pt[1,]<-func_gt2pt_Aa(df_sim_gt[1,])
  
  for(tsim in 2:Tsim){
    df_sim_gt[tsim,1] <- func_iter_Aa_AA(x=df_sim_gt[tsim-1,1], y=df_sim_gt[tsim-1,2], z=df_sim_gt[tsim-1,3])
    df_sim_gt[tsim,2] <- func_iter_Aa_Aa(x=df_sim_gt[tsim-1,1], y=df_sim_gt[tsim-1,2], z=df_sim_gt[tsim-1,3])
    df_sim_gt[tsim,3] <- func_iter_Aa_aa(x=df_sim_gt[tsim-1,1], y=df_sim_gt[tsim-1,2], z=df_sim_gt[tsim-1,3])
    df_sim_pt[tsim,]<-func_gt2pt_Aa(df_sim_gt[tsim,])
  }
  return(list(df_sim_gt=df_sim_gt, df_sim_pt=df_sim_pt))
}

# iteration function
func_iter_Aa_AA <- function(x, y, z){
  return(x^2+x*y+1/4*y^2)
}
func_iter_Aa_Aa <- function(x, y, z){
  return(x*y+2*x*z+1/2*y^2+y*z)
}
func_iter_Aa_aa <- function(x, y, z){
  return(1/4*y^2+y*z+z^2)
}

# GTをPTに変換
func_gt2pt_Aa <- function(df_gt){
  df_pt <- data.frame(matrix(c(df_gt["GT_AA"]+df_gt["GT_Aa"], 
                               df_gt["GT_aa"]), nrow=1))
  colnames(df_pt) <- list_pt_Aa
  return(df_pt)
}

