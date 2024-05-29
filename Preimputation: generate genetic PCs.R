#### Generate kinship relatedness matrix using KING software ####
https://www.kingrelatedness.com/
king -b lastQCb37_KGref_QC.bed --kinship 

#### Run PCA in Genesis ####
library(GWASTools)
library(GENESIS)
library(SNPRelate)
library(umap)

# Read files and prune SNPs
snpgdsBED2GDS(bed.fn = "lastQCb37_KGref_QC.bed", 
              bim.fn = "lastQCb37_KGref_QC.bim", 
              fam.fn = "lastQCb37_KGref_QC.fam", 
              out.gdsfn = "lastQCb37_KGref_QC.gds")

gds <- snpgdsOpen("lastQCb37_KGref_QC.gds")

snpset <- snpgdsLDpruning(gds, method="corr", slide.max.bp=10e6, ld.threshold=sqrt(0.1), verbose=FALSE)

pruned <- unlist(snpset, use.names=FALSE)
length(pruned)

snpgdsClose(gds)

# Read the kinship matrix, and run PC
KINGmat <- kingToMatrix(c("lastQCb37_KGref_QC_kinship.kin0"),estimator = "Kinship")
KINGmat[1:5,1:5]

data1 <- GdsGenotypeReader(filename = "lastQCb37_KGref_QC.gds")
data2 <- GenotypeData(data1)
data2

mypcair <- pcair(data2, kinobj = KINGmat, divobj = KINGmat,
                 snp.include = pruned)

# Extract PCs and save data
PC = data.frame(mypcair$vectors)
PC$Sample_name = row.names(PC)

#save(PC, file = "IMAGEN_QCed_PC.RData")
write.csv(PC, file = "IMAGEN_QCed_PC.csv", na = "", row.names = F)
