##Rebecca Panitch
##10.15.16 
##SNP to Sequence and related Functions 

##To use BSgenome needs to be installed 
source("https://bioconductor.org/biocLite.R")
biocLite("BSgenome")
biocLite("BSgenome.Hsapiens.UCSC.hg38")

##Also need the following libraries
library(BSgenome)
library(BSgenome.Hsapiens.UCSC.hg38)

##Read in database on snps 
setwd("~/desktop")
snps <- read.table("traits.txt", row.names=1, header=TRUE, sep="\t")

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
	human <- getBSgenome(BSgenome.Hsapiens.UCSC.hg38)
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

##function to list all traits that can be inputted into 
list_traits <- function(snps){
	print(rownames(snps))
}

##function to get sequence from snp using only the trait and padding/offset desired
trait_to_sequence <- function(trait, snps, padding){
	snps[] <- lapply(snps, as.character)
	x <- snps[trait,]
	chr <- as.character(x["Chrom"])
	location <- as.numeric(x["Position"])
	poly <- as.character(x["New.N"])
	snp_to_sequence(poly, chr, location, padding)
}
