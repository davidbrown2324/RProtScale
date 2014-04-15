#http://a-little-book-of-r-for-bioinformatics.readthedocs.org/en/latest/src/chapter5.html

retrieveseqs <- function(seqnames,acnucdb)
{
  myseqs <- list()   # Make a list to store the sequences
  require("seqinr")  # This function requires the SeqinR R package
  choosebank(acnucdb)
  for (i in 1:length(seqnames))
  {
    seqname <- seqnames[i]
    print(paste("Retrieving sequence",seqname,"..."))
    queryname <- "query2"
    query <- paste("AC=",seqname,sep="")
    query(`queryname`,`query`)
    seq <- getSequence(query2$req[[1]]) # Makes a vector "seq" containing the sequence
    myseqs[[i]] <- seq
  }
  closebank()
  return(myseqs)
}

printMultipleAlignment <- function(alignment, chunksize=60)
{
  # this function requires the Biostrings package
  require("Biostrings")
  # find the number of sequences in the alignment
  numseqs <- alignment$nb
  # find the length of the alignment
  alignmentlen <- nchar(alignment$seq[[1]])
  starts <- seq(1, alignmentlen, by=chunksize)
  n <- length(starts)
  # get the alignment for each of the sequences:
  aln <- vector()
  lettersprinted <- vector()
  for (j in 1:numseqs)
  {
    alignmentj <- alignment$seq[[j]]
    aln[j] <- alignmentj
    lettersprinted[j] <- 0
  }
  # print out the alignment in blocks of 'chunksize' columns:
  for (i in 1:n) { # for each of n chunks
    for (j in 1:numseqs)
    {
      alnj <- aln[j]
      chunkseqjaln <- substring(alnj, starts[i], starts[i]+chunksize-1)
      chunkseqjaln <- toupper(chunkseqjaln)
      # Find out how many gaps there are in chunkseqjaln:
      gapsj <- countPattern("-",chunkseqjaln) # countPattern() is from Biostrings package
      # Calculate how many residues of the first sequence we have printed so far in the alignment:
      lettersprinted[j] <- lettersprinted[j] + chunksize - gapsj
      print(paste(chunkseqjaln,lettersprinted[j]))
    }
    print(paste(' '))
  }
}


gap_insert <- function(frame,nrows,pos) {
  cols = length(frame)
  name = frame$name[1]
  ins = data.frame(matrix(nrow = nrows,  ncol = cols))
  for(i in seq_len(nrows)){ins[i, ] <- c(0,0,"X",0,0,name)} # no so generic
  
  colnames(ins) <- colnames(frame) # copy of the column names
  
  dat_new <-rbind(frame[1:pos,],ins)
  dat_comb <- rbind(dat_new,frame[(pos+1):(nrow(frame)),])
  frame <- dat_comb
  frame$Position <-as.numeric(seq(1:nrow(frame))) # re number the positions
  frame$Value <-as.numeric(frame$Value) # re numeric ize , this is odd behaviour
  frame$tmpred_io <-as.numeric(frame$tmpred_io)
  frame$tmpred_oi <-as.numeric(frame$tmpred_oi)
  return(frame)
}

seq_string <- function(sequence){
  return (paste(sequence, sep="", collapse="") )
}



