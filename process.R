library(ggplot2)
library(seqinr)
library(RColorBrewer)

source("/home/jon/Programming/R/RProtScale/multiplot.R")
source("/home/jon/Programming/R/RProtScale/alignment.R")
source("/home/jon/Programming/R/RProtScale/protscale.R")
source("/home/jon/Programming/R/RProtScale/tmpred.R")

seqnames <- c("Q2W8R4","Q2W8R8","Q2W8J5")  # Make a vector containing the names of the sequences
seqs <- retrieveseqs(seqnames,"swissprot")  #http://a-little-book-of-r-for-bioinformatics.readthedocs.org/en/latest/src/chapter5.html
hydrop_mmsf <- protscale("Q2W8R4",length(seqs[[1]]))
hydrop_0953 <- protscale("Q2W8R8",length(seqs[[2]]))
hydrop_1026 <- protscale("Q2W8J5",length(seqs[[3]]))

tmpred_mmsf <- tmpred("Q2W8R4",length(seqs[[1]]))
tmpred_0953 <- tmpred("Q2W8R8",length(seqs[[2]]))
tmpred_1026 <- tmpred("Q2W8J5",length(seqs[[3]]))

seq_data_mmsf <- data.frame(c(1:length(seqs[[1]])),seqs[[1]])
seq_data_0953 <- data.frame(c(1:length(seqs[[2]])),seqs[[2]])
seq_data_1026 <- data.frame(c(1:length(seqs[[3]])),seqs[[3]])

colnames(seq_data_mmsf) <- c("Position","AA")
colnames(seq_data_0953) <- c("Position","AA")
colnames(seq_data_1026) <- c("Position","AA")

str <- seq_string(seq_data_mmsf$AA)



data_mmsf <- merge(hydrop_mmsf,seq_data_mmsf, by="Position")
data_0953 <- merge(hydrop_0953,seq_data_0953, by="Position")
data_1026 <- merge(hydrop_1026,seq_data_1026, by="Position")

data_mmsf$tmpred_io <- tmpred_mmsf$io
data_0953$tmpred_io <- tmpred_0953$io
data_1026$tmpred_io <- tmpred_1026$io

data_mmsf$tmpred_oi <- tmpred_mmsf$oi
data_0953$tmpred_oi <- tmpred_0953$oi
data_1026$tmpred_oi <- tmpred_1026$oi

data_mmsf$name <- "Mmsf"
data_0953$name <- "0953"
data_1026$name <- "1026"

#p1 <- ggplot(data_mmsf, aes( Position, Value )) + ggtitle("amb0957 mmsF") + geom_line() + scale_y_continuous("Hydropathy Index",limits=c(-3, 3)) + scale_x_continuous("Sequence Position",limits=c(0, 110))
#p2 <- ggplot(data_1026, aes( Position, Value )) + ggtitle("amb1026") + geom_line() + scale_y_continuous("Hydropathy Index",limits=c(-3, 3)) +  scale_x_continuous("Sequence Position",limits=c(0, 110))
#p3 <- ggplot(data_0953, aes( Position, Value )) + ggtitle("amb0953") + geom_line() + scale_y_continuous("Hydropathy Index",limits=c(-3, 3)) + scale_x_continuous("Sequence Position",limits=c(0, 110))

#p1 <- ggplot(data_mmsf, aes( Position, Value )) + geom_line(data_0953, aes( Position, Value ))

#p1 <- ggplot(data_mmsf) + ggtitle("amb0957 mmsF") + geom_line(aes( Position, tmpred_io ))+ geom_line(aes( Position, tmpred_oi, linetype="dotted" )) + scale_y_continuous() + scale_x_continuous("Sequence Position",limits=c(0, length(seqs[[1]])))
#p2 <- ggplot(data_1026, aes( Position, tmpred_io )) + ggtitle("amb1026") + geom_line() + scale_y_continuous() +  scale_x_continuous("Sequence Position",limits=c(0, length(seqs[[2]])))
#p3 <- ggplot(data_0953, aes( Position, tmpred_io )) + ggtitle("amb0953") + geom_line() + scale_y_continuous() + scale_x_continuous("Sequence Position",limits=c(0, length(seqs[[3]])))
g = brewer.pal(3,"Set1")

#multiplot(p1, p2, p3, cols=1)
#ggplot() + 
 # geom_line(aes(Position, Value, colour=g[1]), data_mmsf) +  
 # geom_line(aes(Position, Value, colour=g[2]), data_1026)

data_mmsf<-gap_insert(data_mmsf,4,6)  #quick method to shift data
# handles the alignment insertion

# combine data
all_data_a <- rbind(data_mmsf,data_0953)
all_data <- rbind(all_data_a,data_1026)

p1 <- ggplot(all_data,aes(x=Position, y = Value, group=name,colour = name)) + ylab("Hydropathy Index") + geom_line() + scale_colour_discrete(name="Protein",
                                                                                                                breaks=c("Mmsf", "0953", "1026"),
                                                                                                                labels=c("Mmsf", "0953", "1026"))
p2 <- ggplot(all_data,aes(x=Position, y = tmpred_io, group=name, colour = name)) + geom_line() + ylab("Score")+ scale_colour_discrete(name="Protein",
                                                                                                                       breaks=c("Mmsf", "0953", "1026"),
                                                                                                                       labels=c("Mmsf", "0953", "1026"))

multiplot(p1, p2, cols=1)

