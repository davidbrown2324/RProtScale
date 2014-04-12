library(RCurl)
library(XML)
library(stringr)
library(plyr)

protscale <- function(seqname,length) {
  base = "http://web.expasy.org/cgi-bin/protscale/protscale.pl?"
  search = "@noft@Hphob.--/--Kyte--&--Doolittle_9_100_linear_no"
  query = paste(seqname,search,sep="")
  url = paste(base,query,sep="")
  page_data = getURL(url) 
  doc = htmlParse(page_data)
  link <-getNodeSet(doc,"//*[@id='sib_body']/ul/li[3]/a")
  address <- xmlSApply(link, xmlGetAttr, "href")
  data_url <- paste("http://web.expasy.org",address,sep="")
  scores = getURL(data_url)
  
  res = strsplit(scores,"\n")[[1]]  #splits data on end of line
  arr = res[c(11:length(res))]  #remove header part
  arr_length = length(arr) # new length of the results array
  delta_l = length-arr_length # difference in lengths
  arr = gsub("Position:","",arr) #remove text parts
  arr = gsub("Score:",",",arr)
  sp = unlist(strsplit(arr,",")) # split on ","
  num = as.numeric(str_extract(sp, "-?\\d+\\.*\\d*")) # find numeric parts
  
  l1 = as.integer(num[1])-1 # this gets the first residue value
  l2 = delta_l-l1 # size of the second window element
  l3 = arr_length+l1+1 # second element starts here
  
  win1 <- array(rbind(c(1:l1),array(0,c(1,l1))))
  win2 <- array(rbind(c(l3:length),array(0,c(1,l2))))
  
  num <- c(win1,num,win2) # join arrays
  mat <- matrix(num, ncol = 2, byrow = TRUE)
  dat <- data.frame(matrix(num, ncol = 2, byrow = TRUE)) # put into frame
  colnames(dat) <- c("Position","Value")
  
  return(dat)
}