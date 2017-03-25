setwd('/home/hduser1/')
library(parallel)
library(magick)
library(png)
dat <- read.csv("flattened.csv")

n_core <- detectCores()-1
cl <- makeCluster(n_core)
clusterExport(cl=cl, "dat")  

result <- parLapply(cl=cl, c(3:15), function(x){
  kmeans(x=dat[,-c(1:3)],centers=x) 
})
names(result) <- c(3:15)

cluster <- lapply(result, function(x){
  yuji <- as.data.frame(cbind(dat[,-c(1:3)], x$cluster))
  names(yuji)[4] <- "class"
  return(yuji)
})

cent <- lapply(result, function(x){
  nolan <- as.data.frame(cbind(1:nrow(x$centers),x$centers))
  names(nolan)[1]<- "class"
  return(nolan)
})
final <- lapply(c(1:18), function(x){
  return(dplyr::inner_join(cluster[[x]], cent[[x]], by = 'class'))
})



