install.packages("igraph")
library("igraph")


g <- random.graph.game(10, p=0.1, direct=F)
plot(g, main = "random graph n = 50, p = 0.1")

components(g)$no

fg <- make_full_graph(5)
plot(fg)

components(fg)


est_p <- 0
N_sim <- 100
N <- 10
p <- .2

for(i in 1:N_sim){
  g <- random.graph.game(N, p=p, direct=F)
  if(components(g)$no==1) est_p<-est_p+1
}
est_p<-est_p/N_sim
est_p



est_p <- 0
est_p1 <- 0
est_p2 <- 0
N_sim <- 1000
N <- 500
p <- log(N)/N
p1 <- 1/sqrt(N)
p2 <- 1/N

for(i in 1:N_sim){
  g <- random.graph.game(N, p=p, direct=F)
  if(components(g)$no==1) est_p<-est_p+1
  
  g1 <- random.graph.game(N, p=p1, direct=F)
  if(components(g1)$no==1) est_p1<-est_p1+1
  
  g2 <- random.graph.game(N, p=p2, direct=F)
  if(components(g2)$no==1) est_p2<-est_p2+1
}
est_p<-est_p/N_sim
est_p1<-est_p1/N_sim
est_p2<-est_p2/N_sim
cat("N=", N,"p=", p, "p1=", p1, "p2=", p2,"  ", est_p, est_p1,est_p2, "\n")

if(N<100){
par(mfrow=c(1,3))
plot(g, main = paste0("n = ", N,", p =", round(p,3), " conn.", as.numeric(components(g)$no)))
plot(g1, main = paste0("n = ", N,", p =", round(p1,3)," conn.", components(g1)$no))
plot(g2, main = paste0("n = ", N,", p =", round(p2,3)," conn.", components(g2)$no))
par(mfrow=c(1,1))
}



