source("pgm/bloodtype-pgm_ABO.R")
source("pgm/bloodtype-pgm_Aa.R")

# initial population  c("GT_AA", "GT_AB", "GT_AO", "GT_BB", "GT_BO", "GT_OO")
percent <-            c(   0.15,    0.25,    0.10,    0.15,    0.10,    0.25)
percent <- percent/sum(percent)
percent

df <- data.frame(matrix(percent, nrow=1)); colnames(df) <- list_gt
func_gt2pt(df)

Tsim <- 200; Nsim <- 1000; 
res_sim <- func_sim_iteration(percent, Tsim, Nsim)


# Aa
# initial population  c("GT_AA", "GT_Aa", "GT_aa")
percent <-            c(   1,    4,    0.45)
percent <- percent/sum(percent)
percent

df <- data.frame(matrix(percent, nrow=1)); colnames(df) <- list_gt
func_gt2pt_Aa(df)

Tsim <- 3;
res_sim <- func_sim_iteration_Aa(percent, Tsim)

df_sim_gt<-res_sim$df_sim_gt
df_sim_pt<-res_sim$df_sim_pt

head(df_sim_gt)
head(df_sim_pt)

df_sim_gt$time <- 0:(Tsim-1)
df_sim_gt_long <- df_sim_gt %>% 
  pivot_longer(cols = list_gt, 
               names_to = "GT",
               values_to = "value")
df_sim_gt_long$value<-as.numeric(df_sim_gt_long$value)
df_sim_pt$time <- 0:(Tsim-1)
df_sim_pt_long <- df_sim_pt %>% 
  pivot_longer(cols = list_pt, 
               names_to = "PT",
               values_to = "value")
df_sim_pt_long$value<-as.numeric(df_sim_pt_long$value)


g_gt_fill <- ggplot(df_sim_gt_long, aes(x = time, y = value, fill = GT))
g_gt_fill <- g_gt_fill + geom_bar(stat = "identity", position = "fill")
g_gt_fill <- g_gt_fill + scale_fill_nejm()

g_gt_plot <- ggplot(df_sim_gt_long, aes(x = time, y = value, color = GT))
g_gt_plot <- g_gt_plot + geom_line()
g_gt_plot <- g_gt_plot + scale_fill_nejm()

g_pt_fill <- ggplot(df_sim_pt_long, aes(x = time, y = value, fill = PT))
g_pt_fill <- g_pt_fill + geom_bar(stat = "identity", position = "fill")
g_pt_fill <- g_pt_fill + scale_fill_nejm()

g_pt_plot <- ggplot(df_sim_pt_long, aes(x = time, y = value, color = PT))
g_pt_plot <- g_pt_plot + geom_line()
g_pt_plot <- g_pt_plot + scale_fill_nejm()


#plot(g_gt_fill)
#plot(g_gt_plot)
#plot(g_pt_fill)
#plot(g_pt_plot)

gridExtra::grid.arrange(g_gt_fill, g_gt_plot, g_pt_fill, g_pt_plot, nrow = 2)



func = function(x,z) func_iter_Aa_aa(x, 1-x-z, z)
x = z = seq(0.01, 1, 0.01)
y = outer(x, z, func)
persp(x, z, y, theta=135, phi=20, expand=0.5, ticktype="detailed") 
contour(x, z, y)


func = function(x,z) 2*sqrt(x*z)
x = z = seq(0.01, 1, 0.01)
y = outer(x, z, func)
y2 = outer(x, z, function(x,z) 1-x-z)
res_3d<-persp(x, z, y, theta=0, phi=20, expand=0.5, ticktype="detailed") 
res_3d<-persp(x, z, y, theta=45, phi=20, expand=0.5, ticktype="detailed") 
res_3d<-persp(x, z, y, theta=135, phi=20, expand=0.5, ticktype="detailed") 
lines(trans3d(x, z, y2, pmat=res_3d))

contour(x, z, y)