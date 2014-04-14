library(RCurl)
library(XML)
library(stringr)
library(plyr)

base = "http://www.ch.embnet.org/cgi-bin/TMPRED_form_parser"
page_data <- postForm(base,
         outmode="html",
         min="17",
         max="33",
         comm="",
         format="SwissProt_ID",
         seq="Q2W8R4", 
         style = "POST")

doc = htmlParse(page_data)
link <-getNodeSet(doc,"//*[@id='sib_body']/ul/li[3]/a")
address <- xmlSApply(link, xmlGetAttr, "href")
data_url <- paste("http://www.ch.embnet.org/",address,sep="")
scores = getURL(data_url)

res = strsplit(scores,"\n")[[1]]  #splits data on end of line
arr = res[c(5:length(res))]  #remove header part
sp = unlist(strsplit(arr,",")) # split on ","
num = as.numeric(str_extract(sp, "-?\\d+\\.*\\d*")) # find numeric parts