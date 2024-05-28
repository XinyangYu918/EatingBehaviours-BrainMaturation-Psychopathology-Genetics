# The following codes were adapted from: https://github.com/vwarrier/ABCD_geneticQC/tree/master/Postimputation

#### Step 1: Create a list of SNPs with acceptable QC stats ####
library(data.table)
for (i in 1:22){
  a = read.table(paste0("chr", i, ".info"), header = T)
  a$Rsq = as.numeric(as.character(a$Rsq))
  b = subset(a, Rsq > 0.4)
  b = subset(b, ALT_Frq > 0.001 & ALT_Frq < 0.999)
  c = b[!duplicated(b$SNP),]
  write.table(c[,1], file = paste0("chr", i, "extract2.txt"), row.names = F, col.names = T, quote = F)
}

# Run in bash
for i in {1..22}; do ./plink --vcf ./chr${i}.dose.vcf.gz --make-bed --out ./chr${i}  --extract ./chr${i}extract_unique.txt --const-fid 0; done

# Remove duplicate SNPs, run in bash
# Loop through chromosomes 1 to 22
for i in {1..22}; do
echo "Processing chromosome $i..."
# Identify and save duplicate variant IDs to exclude
awk '{print $2}' chr${i}.bim | sort | uniq -d > chr${i}_exclude_variants.txt
# Use PLINK to exclude duplicate variants and regenerate the binary files
./plink --bfile chr${i} --exclude chr${i}_exclude_variants.txt --make-bed --out chr${i}_cleaned
# Optional: Replace original files with cleaned versions
# mv chr${i}_cleaned.bed chr${i}.bed
# mv chr${i}_cleaned.bim chr${i}.bim
# mv chr${i}_cleaned.fam chr${i}.fam
echo "Chromosome $i processing completed."
done

# chr${1}_cleaned.bed/bim/fam save the data without any duplicate SNPs


#### Step 2: Update the SNP names ####
library(data.table)
bim22 = fread("./chr22_cleaned.bim")
bim22$CHROM_POS = paste(bim22$V1, bim22$V4, sep = ":")
bim22_update = bim22[, c("CHROM_POS", "V2")]
update = bim22_update

for (i in 1:21){
  bim = fread(paste0("./chr", i, "_cleaned.bim"))
  bim$CHROM_POS = paste(bim$V1, bim$V4, sep = ":")
  bim_update = bim[, c("CHROM_POS", "V2")]
  update = rbind(update, bim_update)
}

plink_recoding = fread("Imputation_QC//plinkrecodingfile.txt")
plink_recoding_file = merge(plink_recoding, update, by.x = "#CHROM:POS:REF:ALT", by.y = "V2")
plink_recoding_file = plink_recoding_file[!duplicated(plink_recoding_file$`#CHROM:POS:REF:ALT`), ]

write.table(plink_recoding_file[,c("#CHROM:POS:REF:ALT", "ID")], file = "plinkrecodingfile2.txt", col.names = T, row.names = F, quote = F)

# run in plink
for i in {1..22}; do ./plink --bfile ./chr${i}  --update-name ./plinkrecodingfile2.txt --make-bed --out ./chr${i}_v2; done

#### Step 3: Update family names ####
library(data.table)
library(tidyr)

fileimputed = fread("./chr1_v2.fam")
fileimputed$oldFID = fileimputed$V1
fileimputed$oldIID = fileimputed$V2

fileimputed = fileimputed %>% separate(V2, into = c('FID', 'IID'), sep = "_")
write.table(fileimputed[,c("oldFID", "oldIID", "FID", "IID")], file = "updatefamenames.txt", row.names = F, col.names = F, quote = F)


#### Step 4: Repeat the whole process for the X chromosome ####
a = fread("chrX.info", header = T)
a$Rsq = as.numeric(as.character(a$Rsq))
b = subset(a, Rsq > 0.4)
b = subset(b, ALT_Frq > 0.001 & ALT_Frq < 0.999)
write.table(b[,1], file = "chrXextract.txt", row.names = F, col.names = T, quote = F)

# run in bash
./plink --vcf ./chrX.dose.vcf.gz --make-bed --out ./chrX  --extract ./chrXextract.txt --const-fid 0

# Remove duplicate SNPs, run in bash
awk '{print $2}' chrX.bim | sort | uniq -d > chrX_exclude_variants.txt
# Use PLINK to exclude duplicate variants and regenerate the binary files
./plink --bfile chrX --exclude chrX_exclude_variants.txt --make-bed --out chrX_cleaned

library(data.table)
bimX = fread("./chrX_cleaned.bim")
bimX$CHROM_POS = paste(bimX$V1, bimX$V4, sep = ":")
bimX_update = bimX[, c("CHROM_POS", "V2")]
update = bimX_update
update$CHROM_POS = gsub("23:", "X:",update$CHROM_POS)

plink_recoding = fread("plinkrecodingfile.txt")
plink_recoding_file = merge(plink_recoding, update, by.x = "#CHROM:POS:REF:ALT", by.y = "V2")
plink_recoding_file = plink_recoding_file[!duplicated(plink_recoding_file$`#CHROM:POS:REF:ALT`), ]

write.table(plink_recoding_file[,c("#CHROM:POS:REF:ALT", "ID")], file = "plinkrecodingfile2_forX.txt", col.names = T, row.names = F, quote = F)

# Run in bash
./plink --bfile ./chrX_cleaned  --update-name plinkrecodingfile2_forX.txt --make-bed --out ./chrX_v2
./plink --bfile ./chrX_v2  --update-ids updatefamenames.txt --make-bed --out ./chrX_v3
./plink --bfile ./chrX_v3  --recode --out ./chrX_v4

data1 = fread("chrX_v3.bim")
subset(data1, V1 == X)
write.table(data1, file = "chrX_hg19.bim"), row.names = F, col.names = F, quote = F)

#### Step 5: combine into 1 file for polygenic scoring ####
for i in {1..22}; do ./plink --bfile chr${i}_v3 --exclude allchrs-merge.missnp --maf 0.001 --make-bed --out ABCD_chr${i}_hg19_v2 --threads 10; done
./plink --bfile ABCD_chr1_hg19_v2 --merge-list ABCD_merge.txt --maf 0.001 --make-bed --out ABCD_hg19_allchrs --threads 10

for i in {1..22}; do rm ABCD_chr${i}_hg19_v2*; done

#Retain European
./plink --bfile ABCD_hg19_allchrs --keep ~/ABCD/ABCDgenotype/GWAS/QC4_european_grm.grm.id --make-bed --out ABCD_hg19_allchrs_europeanonly --maf 0.001 --threads 15




#### for updating SNP names for lastQCb37 files
plink_recoding = fread("plinkrecodingfile_format2.txt")
lastQCb37.bim <- fread("lastQCb37.bim")
plink_recoding_file = merge(plink_recoding, lastQCb37.bim, by.x = "#CHROM:POS", by.y = "V2")
plink_recoding_file = plink_recoding_file[!duplicated(plink_recoding_file$`#CHROM:POS`), ]
write.table(plink_recoding_file[,c("#CHROM:POS", "ID")], file = "plinkrecodingfile2_format2.txt", col.names = T, row.names = F, quote = F)
