##Rebecca Panitch
##10.15.16 
##SNP to Sequence Function 

##To use BSgenome needs to be installed 
source("https://bioconductor.org/biocLite.R")
biocLite("BSgenome")
biocLite("BSgenome.Hsapiens.UCSC.hg38")

##Also need the following libraries
library(BSgenome)
library(BSgenome.Hsapiens.UCSC.hg38)

##print results from the snp_to_sequence function
printresults <- function(seq, padding){
	print(paste("Result:", seq))
	print(paste("Offset:", padding))
}

## polymorphism = "A", "C", "G", or "T"
## chr = "chr#" (chromosome snp is on)
## bp = #  (position of basepair where snp appears)
## padding = # (how many bp to flank snp region with) 

snp_to_sequence <- function(polymorph, chr, bp, padding){
	human <- getBSgenome(BSgenome.Hsapiens.UCSC.hg19)
	##beginning point of sequence
	start <- (bp-padding)
	##ending point of sequence
	end <- (bp+padding)
	seq <- getSeq(Hsapiens, chr, start, end)
	
	##Replace SNP with correct polymorphism
	left <- substr(seq, 1, padding)
	right <- substr(seq, padding+2, length(seq))
	new_seq <- paste(left, polymorph, right, sep="")
	printresults(new_seq, padding)
}



snp_to_sequence("C", "chr19", 44908684, 3)

