#setting paths
setwd("/home/ibab/system_bio/microarray_q1")
data_files=("/home/ibab/sr3sem/GSE77298/GSE77298_hgu133plus2")
Affy_chip = "hgu133plus2"
# Geo number
GEO_number = "GSE77298"
infile_condition = "covdesc_GSE77298_GPL570"
covdesc_file = paste(infile_condition,".txt", sep="")
infile_dir = data_files
#loading libraries
library(matrixStats)
library(affy)
library(simpleaffy)
library(multtest)

raw.data <- read.affy(covdesc_file,path=infile_dir) #/data_files/covdesc_all.txt
# t. test between control and RA
test_between = c("RA","control")
# Algorithm for probe summarization
probe_summary_algorithm = "rma" 
# Normalize using the probe summary algorithm
normalized_data  <- call.exprs(raw.data, probe_summary_algorithm, sc=500, method=NA)
# Get expression values from normalized data.
expval <- exprs(normalized_data)
prbnames <- rownames(expval)
#Identify the control and disease samples in the expression values matrix for differential expression analysis.
xRA = which(raw.data@phenoData@data$source_name == test_between[1])
xcont = which(raw.data@phenoData@data$source_name == test_between[2])
# Number of control and treatment samples
control_samples = length(xcont)
RA_samples = length(xRA)
RA_expval <- expval[,xRA]
cont_expval <- expval[,xcont]

M <- as.matrix(data.frame(2^expval))
# since expval is log2 transformed
# row_t_test is the function to perform row-wise t test for each gene(each row)
row_t_test <- function(x){ t.test(x[xcont],x[xRA], paired=FALSE, var.equal=FALSE)} 
res <- apply(M, 1, row_t_test)
# res is a list, subset the list to obtain p values
pval<-as.vector(vapply(res, "[[", 3, i = "p.value"))
names(pval) = row.names(M)

# compute Fold change = RA/control
# creating empty vectors of length equals number of rows in expval.
RA_data =  rep(0, nrow(expval))
control_data   =  rep(0,nrow(expval))

# Summing all columns of control
for(i in xcont)
{
  control_data = control_data + 2^expval[,i]
}

# summing all columns of RA
for(i in xRA)
{
  RA_data = RA_data + 2^expval[,i]
}

RA_linear_mean = (RA_data/RA_samples)
control_linear_mean = (control_data/control_samples)
# Fold change is RA column divided by control column. We get fold change for all genes.
FC_linear = as.vector(RA_linear_mean/control_linear_mean)
FC_log2 = log2(FC_linear)
FC_linear = round(FC_linear, 4)
FC_log2 = round(FC_log2,4)

# adjust Welch.t.test pvals - FDR (BH)
genes.present = prbnames
pvals.uncorrected.genes.present = pval[genes.present]
pvals.fdr.genes.present = p.adjust(pvals.uncorrected.genes.present, method = "fdr")
pvals.fdr.genes.present = round(pvals.fdr.genes.present, 4)

# Combine the data into a dataframe and annotate with gene ids.
all.data.rma = data.frame(probeset=names(pval),
                          RA_linear_mean,
                          control_linear_mean,
                          FC_linear,
                          FC_log2,
                          pval,
                          FDR_corrected=pvals.fdr.genes.present)

# Upload annotation file with probeset to gene id mappings.
DAVID_ID <- read.delim(paste("/home/ibab/system_bio/28_4_19/hgu133plus2_DAVIDID_SYM.tsv"),
                       header=T,
                       sep="\t")

# Add the gene id and gene name column to expression values dataframe and differential expression dataframe.
expframe <- as.data.frame(expval)
expframe$probeset <- rownames(expframe)
expframe$DAVID <- DAVID_ID[match(expframe$probeset,DAVID_ID$From),]$To
expframe$SYM <- DAVID_ID[match(expframe$probeset,DAVID_ID$From),]$SYM
expframe$genename_DAVID <- DAVID_ID[match(expframe$probeset,DAVID_ID$From),]$Gene.Name

all.data.rma$DAVID <- DAVID_ID[match(all.data.rma$probeset,DAVID_ID$From),]$To
all.data.rma$SYM <- DAVID_ID[match(all.data.rma$probeset,DAVID_ID$From),]$SYM
all.data.rma$genename_DAVID <- DAVID_ID[match(all.data.rma$probeset,DAVID_ID$From),]$Gene.Name

# Remove control probes and unannotated probes.
# identify NA probes in the expression values dataframe
expframe_annotated = expframe[!is.na(expframe$DAVID),]

# subset annotated probes and create new expval matrix with only annotated probes
expval_annotated = as.matrix(subset(expframe_annotated,select=-c(probeset,DAVID,genename_DAVID,SYM)))

# removes control probes: they start with AFFX in probeset ID
ctrlprobe_expval_annotated <- grep("^AFFX",rownames(expval_annotated))
expval_final <- expval_annotated[-ctrlprobe_expval_annotated,]

# remove NA
all.data.rma.annot0 = all.data.rma[!is.na(all.data.rma$DAVID),]

# remove ctrl probes
ctrlprobe_alldata <- grep("^AFFX",all.data.rma.annot0$probeset)
all.data.rma.annot <- all.data.rma.annot0[-ctrlprobe_alldata,]

# Filter the significantly differentially expressed genes.
# FDR pvalue <= 0.1
FDRcut=0.1
sig.all.data.rma0 = all.data.rma.annot[which(all.data.rma.annot$FDR_corrected <= FDRcut),]

# Filter top 25 up and down regulated genes.

# downregulated
down.all.data.rma = sig.all.data.rma0[order(sig.all.data.rma0$FC_log2),][1:25,]

# upregulated
up.all.data.rma = sig.all.data.rma0[order(-sig.all.data.rma0$FC_log2),][1:25,]

