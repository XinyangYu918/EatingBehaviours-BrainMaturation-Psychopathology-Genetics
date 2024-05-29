## In R, check heterozygosity
het = read.table("check_het_lastQCb37_MDSremoved_v2.het", header = T)
het$HET = (het$N.NM. - het$O.HOM.)/het$N.NM. #create heterozygosity stats
mean = mean(het$HET)
sd = sd(het$HET)
het$Z = (het$HET - mean)/sd #create Z scores of heterozygosity
hetoutlier = subset(het, abs(Z) > 3)
failedsample = hetoutlier[,c(1:2)]
write.table(failedsample, file ="het_failedsample.txt", row.names = F, col.names = T, quote = F)
