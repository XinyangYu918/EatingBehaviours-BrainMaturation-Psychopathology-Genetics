# To compare demographics among three groups
library(dplyr)
library(MASS)

df <- read.csv("demographics.csv")
df$age1 <- df$age1/365.25
df$age2 <- df$age2/365.25
df$age3 <- df$age3/365.25
df$age4 <- df$age4/365.25
df$IQ <- (df$IQ)/2

df_stats <- df %>%
  group_by(Cluster) %>%
  summarise(
    age1_mean = mean(age1, na.rm = TRUE),
    age1_sd = sd(age1, na.rm = TRUE),
    age2_mean = mean(age2, na.rm = TRUE),
    age2_sd = sd(age2, na.rm = TRUE),
    age3_mean = mean(age3, na.rm = TRUE),
    age3_sd = sd(age3, na.rm = TRUE),
    age4_mean = mean(age4, na.rm = TRUE),
    age4_sd = sd(age4, na.rm = TRUE),
    BMI_mean = mean(BMI, na.rm = TRUE),
    BMI_sd = sd(BMI, na.rm = TRUE),
    PDS_mean = mean(PDS, na.rm = TRUE),
    PDS_sd = sd(PDS, na.rm = TRUE),
    IQ_mean = mean(IQ, na.rm = TRUE),
    IQ_sd = sd(IQ, na.rm = TRUE),
    EA_mean = mean(EA, na.rm = TRUE),
    EA_sd = sd(EA, na.rm = TRUE),
    CR_mean = mean(CR, na.rm = TRUE),
    CR_sd = sd(CR, na.rm = TRUE),
    EE_mean = mean(EE, na.rm = TRUE),
    EE_sd = sd(EE, na.rm = TRUE),
    BMIPGS_mean = mean(BMIPGS, na.rm = TRUE),
    BMIPGS_sd = sd(BMIPGS, na.rm = TRUE)
  )

age1 <- aov(age1 ~ Cluster, data=df)
age2 <- aov(age2 ~ Cluster, data=df)
age3 <- aov(age3 ~ Cluster, data=df)
age4 <- aov(age4 ~ Cluster, data=df)
BMI <- aov(BMI ~ Cluster, data=df)
PDS <- aov(PDS ~ Cluster, data=df)
IQ <- aov(IQ ~ Cluster, data=df)
EA <- aov(EA ~ Cluster, data=df)
CR <- aov(CR ~ Cluster, data=df)
EE <- aov(EE ~ Cluster, data=df)
UE <- aov(UE ~ Cluster, data=df)
BMIPGS <- aov(BMIPGS ~ Cluster, data=df)

summary(BMIPGS,digits=6)
pairwise_results <- pairwise.t.test(df$BMIPGS, df$Cluster, p.adjust.method = "bonferroni")
print(pairwise_results, digits = 4)

# Create a contingency table
contingency_table <- table(df$Cluster, df$sex)
chi_square_result <- chisq.test(contingency_table)
print(chi_square_result)

# Post-hoc analysis
pairwise_chisq <- function(table, p.adjust.method = "bonferroni") {
  # Get the levels of the factor
  levels <- rownames(table)
  n <- length(levels)
  # Initialize a matrix to store p-values
  p_values <- matrix(NA, n, n, dimnames = list(levels, levels))
  # Perform pairwise comparisons
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      sub_table <- table[c(i, j), ]
      test <- chisq.test(sub_table)
      p_values[i, j] <- test$p.value
      p_values[j, i] <- test$p.value
    }
  }
  
  # Adjust p-values for multiple comparisons
  p_values <- p.adjust(p_values, method = p.adjust.method)
  return(p_values)
}

pairwise_p_values <- pairwise_chisq(contingency_table)
print(pairwise_p_values)
