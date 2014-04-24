library(foreach)

alignplot <- function(data,seq_list,size) {
  plot.new()
  
  chunksize = 30
  chunkspace = 18
  seq_gap = 4
  
  #step = 0.5
  cex = 0.6
  plotsize = 40
  margin = 0
  sy = 40
  
  #can I do this with castor reshape?
  l <- length(seq_list)
  t = 1:l
  seq_dat <- data.frame(matrix(NA, nrow = size, ncol = l)) # init frame
  foreach(t=1:l) %do% {
    seq_dat[,t] <- subset(all_data, name==seq_list[t], select = AA)
  }
  colnames(seq_dat) <- seq_list
  
  # define size of the plot window with margin and the set of letter spacings
  plot.window(xlim=c(-1*plotsize,plotsize),ylim=c(-1*plotsize,plotsize), pty="s")
  a = seq(-1*plotsize+margin,plotsize-margin,length.out=chunksize)

  # calculate variables for the chunking
  blocks = nrow(sub)
  segments = floor(blocks/chunksize)
  complete = segments*chunksize
  rem = blocks%%chunksize
  end = complete + rem
  
  # the main loop
  
  foreach(s=1:segments) %do% {
   foreach(t=1:chunksize) %do% {
     pos.x = a[t]
     foreach(u=1:l) %do% {
      txt <- seq_dat[chunksize*(s-1)+t,u]       
      pos.y = sy-s*chunkspace-(u-1)*seq_gap
      text(pos.x,pos.y,pos=4,txt,cex=cex)
     }
   }
  }
  
  foreach(t=1:rem) %do% {
    pos.x = a[t]
    foreach(u=1:l) %do% {
      txt <- seq_dat[complete+t,u] 
      pos.y = sy-(segments+1)*chunkspace-(u-1)*seq_gap
      text(pos.x,pos.y,pos=4,txt,cex=cex)
    }
  }
  
  rect(-1*plotsize, -1*plotsize, plotsize, plotsize) # transparent
}