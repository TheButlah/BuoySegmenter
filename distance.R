df = read.csv("pandas.csv", sep = ',')

colnames(df) = c("frame", "x", "y", "l", "a", "b")
##df = df[order(df$frame),]
df = df[order(df$y),]
for(i in 1:unique(df$frame)){
  dfSub = df[which(df$frame == i),]
  for(j in 1:(512*512)){
    if(43<=dfSub$l[j]<=63 & 70 <= dfSub$a[j] <= 90 & 57 <= dfSub$b[j] <= 77)
    {
      pxRL = dfSub$x[j]
    break
    }
    if(87<=dfSub$l[j]<=107 & -31 <= dfSub$a[j] <= -11 & 84 <= dfSub$b[j] <= 104)
    {
      pxYL = dfSub$x[j]
      break
    }
  }
  for(j in (512*512):1){
    if(43<=dfSub$l[j]<=63 & 70 <= dfSub$a[j] <= 90 & 57 <= dfSub$b[j] <= 77)
    {
      pxRR = dfSub$x[j]
      break
    }
    if(87<=dfSub$l[j]<=107 & -31 <= dfSub$a[j] <= -11 & 84 <= dfSub$b[j] <= 104)
    {
      pxYR = dfSub$x[j]
      break
    }
  }
  
}
df = df[order(df$x),]
for(i in 1:unique(df$frame)){
  dfSub = df[which(df$frame == i),]
  for(j in 1:(512*512)){
    if(43<=dfSub$l[j]<=63 & 70 <= dfSub$a[j] <= 90 & 57 <= dfSub$b[j] <= 77)
    {
      pxRT = dfSub$y[j]
      break
    }
    if(87<=dfSub$l[j]<=107 & -31 <= dfSub$a[j] <= -11 & 84 <= dfSub$b[j] <= 104)
    {
      pxYT = dfSub$y[j]
      break
    }
  }
  for(j in (512*512):1){
    if(43<=dfSub$l[j]<=63 & 70 <= dfSub$a[j] <= 90 & 57 <= dfSub$b[j] <= 77)
    {
      pxRB = dfSub$y[j]
      break
    }
    if(87<=dfSub$l[j]<=107 & -31 <= dfSub$a[j] <= -11 & 84 <= dfSub$b[j] <= 104)
    {
      pxYB = dfSub$y[j]
      break
    }
  }
}

redDistance = function(pxRL, pxRR, pyRT, pyRB){
  a = 44.0
  pxR = (pxRR + pxRL)/2
  pyR = (pyRT + pyRB)/2
  w = 0.2*1600/(pxRR-pxRL)
  xRh = (pxR - 800)* 0.2/(pxRR-pxRL)
  xRv = (pYR - 600)*0.2/(pyRT-pyRB)
  y = xRh/(sin(2*a*xRh*pi)/(180*w))
  return(c(xRh, xRv, xRh/(tan(2*a*xRh*pi)/(180*w))))
}

yellowDistance = function(pxYL, pxYR, pyYT, pyYB){
  a = 44.0
  pxY = (pxYR + pxYL)/2
  pyY = (pyYT + pyYB)/2
  w = 0.2*1600/(pxYR-pxYL)
  xYh = (pxY - 800)* 0.2/(pxYR-pxYL)
  xYv = (pYR - 600)*0.2/(pyYT-pyYB)
  y = xYh/(sin(2*a*xYh*pi)/(180*w))
  return(c(xYh, xYv, xYh/tan(2*a*xYh*pi)/(180*w)))
}

frame = data.frame(unique(df$frame))
for(i in 1:unique(df$frame)){
  red = redDistance(pxRL, pxRR, pyRT, pyRB)
  yellow = yellowDistance(pxYL, pxYR, pyYT, pyYB)
  frame$redDist[i] = red
  frame$yellDist[i] = yellow
}

return(frame)




