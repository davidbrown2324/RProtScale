library(RCurl)
library(XML)
library(stringr)
library(plyr)

tmpred <- function(seqname,length) {
base = "http://www.ch.embnet.org/cgi-bin/TMPRED_form_parser"
page_data <- postForm(base,
         outmode="html",
         min="17",
         max="33",
         comm="",
         format="SwissProt_ID",
         seq=seqname, 
         style = "POST")

doc = htmlParse(page_data)
link <-getNodeSet(doc,"//*[@id='sib_body']/ul/li[3]/a")
address <- xmlSApply(link, xmlGetAttr, "href")
data_url <- paste("http://www.ch.embnet.org/",address,sep="")
scores = getURL(data_url)

res = strsplit(scores,"\n")[[1]]  #splits data on end of line
arr = res[c(5:length(res))]  #remove header part
sp = unlist(strsplit(arr,",")) # split on ","
#num = str_extract(sp, "-?\\d+\\.*\\d*") # find numeric parts
num = as.numeric(str_extract(sp, "-?\\d+\\.*\\d*")) # find numeric parts
dat <- data.frame(matrix(num, ncol = 3, byrow = TRUE)) # put into frame
#dat[,2:3] <- sapply(dat[,2:3], as.numeric)
colnames(dat) <- c("AA","io","oi")

return(dat)
}
