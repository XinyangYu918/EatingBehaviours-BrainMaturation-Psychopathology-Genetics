#### Generate kinship relatedness matrix using KING software ####
https://www.kingrelatedness.com/
king -b lastQCb37_KGref_QC.bed --kinship 

#### Run PCA in Genesis ####
library(GWASTools)
library(GENESIS)
library(SNPRelate)
library(umap)
library(ggplot2)
library(data.table)

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
PC <- data.frame(mypcair$vectors)
PC$Sample_name <- row.names(PC)

# Load PC data and additional population files to determine genetic ancestry
populationfile <- fread("IMAGEN_1KG_population.txt")
setnames(PC, "Sample_name", "Sample")
populationfile <- populationfile[!duplicated(populationfile$Sample),]

PC_withancestry <- merge(populationfile, PC, by = "Sample")
PC_withancestry <- unique(PC_withancestry)

# First identify genetic clusters only in the subset of 1 KG data
PC_withancestry_1KG <- subset(PC_withancestry, Dataset == "1000G")

# Keep only the first five PCs and make plots
PC_forumap_1KG <- PC_withancestry_1KG[,c(5:9)]
PC_forumap_labels_1KG <- PC_withancestry_1KG[,c(1:4)]
PC_umap_1KG <- umap(PC_forumap_1KG)

PC_umap_layout_1KG <- PC_umap_1KG$layout
PC_umap_layout_1KG <- cbind(PC_umap_layout_1KG, PC_forumap_labels_1KG)
ggplot(PC_umap_layout_1KG,aes(x=V1,y=V2,color=Superpopulation)) + geom_point(aes(shape = Superpopulation, colour = Superpopulation)) + scale_shape_manual(values=c(1:15))

# Keep only the first eight PCs and make plots
PC_forumap_1KG <- PC_withancestry_1KG[,c(5:12)]
PC_forumap_labels_1KG <- PC_withancestry_1KG[,c(1:4)]
PC_umap_1KG <- umap(PC_forumap_1KG)

PC_umap_layout_1KG <- PC_umap_1KG$layout
PC_umap_layout_1KG <- cbind(PC_umap_layout_1KG, PC_forumap_labels_1KG)
ggplot(PC_umap_layout_1KG,aes(x=V1,y=V2,color=Superpopulation)) + geom_point(aes(shape = Superpopulation, colour =Superpopulation)) + scale_shape_manual(values=c(1:15))

# Keep only the first ten PCs and make plots
PC_forumap_1KG <- PC_withancestry_1KG[,c(5:14)]
PC_forumap_labels_1KG <- PC_withancestry_1KG[,c(1:4)]
PC_umap_1KG <- umap(PC_forumap_1KG)

PC_umap_layout_1KG <- PC_umap_1KG$layout
PC_umap_layout_1KG <- cbind(PC_umap_layout_1KG, PC_forumap_labels_1KG)
ggplot(PC_umap_layout_1KG,aes(x=V1,y=V2,color=Superpopulation)) + geom_point(aes(shape = Superpopulation, colour = Superpopulation)) + scale_shape_manual(values=c(1:15))

# Then project IMAGEN data to the 1000G data
PC_withancestry_no1KG <- subset(PC_withancestry, Dataset == "IMAGEN")
PC_forumap_no1KG <- PC_withancestry_no1KG[,c(5:9)]
PC_forumap_labels_no1KG <- PC_withancestry_no1KG[,c(1:4)]
PC_umap_no1KG <- predict(PC_umap_1KG, PC_forumap_no1KG)
PC_umap_layout_no1KG <- cbind(PC_umap_no1KG, PC_forumap_labels_no1KG)

PC_all_umap <- rbind(PC_umap_layout_1KG, PC_umap_layout_no1KG)

ggplot(PC_all_umap,aes(x=V1,y=V2,colour = Superpopulation)) + geom_point(aes(shape = Superpopulation, colour = Superpopulation)) + scale_shape_manual(values=c(1:15))
p = ggplot(PC_all_umap,aes(x=V1,y=V2,color=Superpopulation)) + geom_point(aes(shape = Superpopulation, colour = Population)) + scale_shape_manual(values=c(1:15))

p + theme(panel.grid.minor = element_line(colour="white", size=0.5)) +  
scale_y_continuous(minor_breaks = seq(-15 , 15, 0.5), 
                   breaks = seq(-15 , 15, 1)) + 
scale_x_continuous(minor_breaks = seq(-15 , 15, 0.5), breaks = seq(-15 , 15, 1))

# To identify outliers from EUR superpopulation in IMAGEN
ggplot(PC_all_umap,aes(x=V1,y=V2,colour = Superpopulation)) + geom_point(aes(shape = Superpopulation, colour = Superpopulation)) + scale_shape_manual(values=c(1:15))
outlier_chunk1 <- subset(PC_umap_layout_no1KG, V1 > 7 & V1 < 20 & V2 > -5 & V2 < 0 )
outlier_chunk2 <- subset(PC_umap_layout_no1KG, V1 > 10 & V1 < 12 & V2 > -10 & V2 < -5 )
outlier_chunk3 <- subset(PC_umap_layout_no1KG, V1 > 25 & V1 < 30 & V2 > -20 & V2 < -10 )
outlier_chunk <- rbind(outlier_chunk1, outlier_chunk2, outlier_chunk3)
ggplot(outlier_chunk,aes(x=V1,y=V2,color=Superpopulation)) + geom_point(aes(shape = Superpopulation, colour = Superpopulation)) + scale_shape_manual(values=c(1:15))

# Check and exclude outlier participants from the subsequent analysis
write.table(outlier_chunk$Sample, file = "./Genetic_PC_outliers.txt", quote = F, col.names = F)
