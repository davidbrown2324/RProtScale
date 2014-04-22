library(foreach)

alignplot <- function(data) {
  plot.new()
  
  chunksize = 30
  chunkspace = 8
  
  step = 0.9
  cex = 0.8
  plotsize = 20
  margin = 0
  sy = 20
  
  
  plot.window(xlim=c(-1*plotsize,plotsize),ylim=c(-1*plotsize,plotsize), pty="s")
  a = seq(-1*plotsize+margin,plotsize-margin,length.out=chunksize)

  blocks = length(data)
  segments = floor(blocks/chunksize)
  complete = segments*chunksize
  rem = blocks%%chunksize
  end = complete + rem
  print(segments)
  print(end)
  
  foreach(s=1:segments) %do% {
   foreach(t=1:chunksize) %do% {
    txt <- data[chunksize*(s-1)+t] 
    text(a[t],sy-s*chunkspace,txt,cex=cex)
   }
  }
  
  foreach(t=1:rem) %do% {
    txt <- data[complete+t] 
    text(a[t],sy-(segments+1)*chunkspace,txt,cex=cex)
  }
  

  
  box()
  
}