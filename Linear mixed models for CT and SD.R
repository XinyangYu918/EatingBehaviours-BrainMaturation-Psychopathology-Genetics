#### Run linear mixed models for cortical thickness and sulcal depth, using data derived from DK40 atlas ####
library(lme4)
library(lmerTest)
library(ggplot2)
library(dplyr)

df <- read.csv("ROI_aparc_DK40_thickness.csv")
df <- df[,-c(3,4,11,12)]
df$mean <- rowMeans(df[,c(3:70)])
list <- read.csv("all_participants.csv")

list <- list %>%
  mutate(site = case_when(
    s1 == 1 ~ 1,
    s2 == 1 ~ 2,
    s3 == 1 ~ 3,
    s4 == 1 ~ 4,
    s5 == 1 ~ 5,
    s6 == 1 ~ 6,
    s7 == 1 ~ 7,
    TRUE ~ 8
  ))

df <- subset(df, ID %in% list$ID)
df <- merge(df, list, by = "ID")

# try neurocombat
#df_subset <- df[,c(3:70)]
#df_subset2 <- df[,c(80)]
#CT <- data.matrix(t(df_subset))
#BATCH <- data.matrix(t(df_subset2))
#data.harmonized <- neuroCombat(dat=CT, batch=BATCH, parametric=FALSE)
#data.original <- data.harmonized$dat.original
#data.standard <- data.harmonized$dat.standardized

# LMM analysis between group1 and 2
df_1_2 <- subset(df, group !=3)
df_1_2$ParticipantID <- rep(1:542, each = 2)
df_1_2 <- df_1_2[,c(1,81,80,2,3:79)]

results_1_2 <- data.frame(ROI=colnames(df_1_2)[5:73])

for (c in 5:73)
{
  model_1_2 <- lmer(df_1_2[,c] ~ group * time + sex + (1|site/ParticipantID), REML = T,data = df_1_2)
  # Extract the summary
  summary_model_1_2 <- summary(model_1_2)
  # Extract the t value and p-value for group:time interaction
  beta_value_1_2 <- summary_model_1_2$coefficients[,"Estimate"]["group:time"]
  se_value_1_2 <- summary_model_1_2$coefficients[,"Std. Error"]["group:time"]
  t_value_1_2 <- summary_model_1_2$coefficients[,"t value"]["group:time"]
  p_value_1_2 <- summary_model_1_2$coefficients[,"Pr(>|t|)"]["group:time"]
  # Store the t and p values
  results_1_2[c-4,2] <- beta_value_1_2
  results_1_2[c-4,3] <- se_value_1_2
  results_1_2[c-4,4] <- t_value_1_2
  results_1_2[c-4,5] <- p_value_1_2
}

names(results_1_2) <- c("ROI","beta","se","t","p")
results_1_2$fdr.p = p.adjust(results_1_2$p, method = "fdr")
results_1_2$bh.p = p.adjust(results_1_2$p, method = "bonferroni")

sum(results_1_2$fdr.p<0.05)
sum(results_1_2$bh.p<0.05)

write.csv(results_1_2, file = "LMM_results/CT/ANvsBN.csv", na = "", row.names = F)

# LMM analysis between groups 1 and 3
df_1_3 <- subset(df, group !=2)
df_1_3$ParticipantID <- rep(1:713, each = 2)
df_1_3 <- df_1_3[,c(1,81,80,2,3:79)]

results_1_3 <- data.frame(ROI=colnames(df_1_3)[5:73])
for (c in 5:73)
{
  model_1_3 <- lmer(df_1_3[,c] ~ group * time + sex + (1|site/ParticipantID), REML = T,data = df_1_3)
  summary_model_1_3 <- summary(model_1_3)
  beta_value_1_3 <- summary_model_1_3$coefficients[,"Estimate"]["group:time"]
  se_value_1_3 <- summary_model_1_3$coefficients[,"Std. Error"]["group:time"]
  t_value_1_3 <- summary_model_1_3$coefficients[,"t value"]["group:time"]
  p_value_1_3 <- summary_model_1_3$coefficients[,"Pr(>|t|)"]["group:time"]
  results_1_3[c-4,2] <- beta_value_1_3
  results_1_3[c-4,3] <- se_value_1_3
  results_1_3[c-4,4] <- t_value_1_3
  results_1_3[c-4,5] <- p_value_1_3
}

names(results_1_3) <- c("ROI","beta","se","t","p")
results_1_3$fdr.p = p.adjust(results_1_3$p, method = "fdr")
results_1_3$bh.p = p.adjust(results_1_3$p, method = "bonferroni")

sum(results_1_3$fdr.p<0.05)
sum(results_1_3$bh.p<0.05)

write.csv(results_1_3, file = "LMM_results/CT/ANvsHC.csv", na = "", row.names = F)

# LMM analysis between groups 2 and 3
df_2_3 <- subset(df, group !=1)
df_2_3$ParticipantID <- rep(1:643, each = 2)
df_2_3 <- df_2_3[,c(1,81,80,2,3:79)]

results_2_3 <- data.frame(ROI=colnames(df_2_3)[5:73])

for (c in 5:73)
{
  model_2_3 <- lmer(df_2_3[,c] ~ group * time + sex + (1|site/ParticipantID), REML = T,data = df_2_3)
  summary_model_2_3 <- summary(model_2_3)
  beta_value_2_3 <- summary_model_2_3$coefficients[,"Estimate"]["group:time"]
  se_value_2_3 <- summary_model_2_3$coefficients[,"Std. Error"]["group:time"]
  t_value_2_3 <- summary_model_2_3$coefficients[,"t value"]["group:time"]
  p_value_2_3 <- summary_model_2_3$coefficients[,"Pr(>|t|)"]["group:time"]
  2_3_or_value <- exp(summary_model_2_3$coefficients[,"Estimate"]["group:time"])
  results_2_3[c-4,2] <- beta_value_2_3
  results_2_3[c-4,3] <- se_value_2_3
  results_2_3[c-4,4] <- t_value_2_3
  results_2_3[c-4,5] <- p_value_2_3
  results_2_3[c-4,6] <- 2_3_or_value 
}

names(results_2_3) <- c("ROI","beta","se","t","p","OR")
results_2_3$fdr.p = p.adjust(results_2_3$p, method = "fdr")
results_2_3$bh.p = p.adjust(results_2_3$p, method = "bonferroni")

sum(results_2_3$fdr.p<0.05)
sum(results_2_3$bh.p<0.05)

write.csv(results_2_3, file = "LMM_results/CT/BNvsHC.csv", na = "", row.names = F)

#### Plot trajectories of CT from ages 14 to 23 ####
df <- read.csv("ROI_aparc_DK40_thickness.csv")
list <- read.csv("all_participants.csv")
df <- subset(df, ID %in% list$ID)
df <- merge(df, list, by = "ID")
df <- df[,-c(3,4,11,12)]

# For groups1 and 2, the significant region is rfrontalpole
res_1_2 <- data.frame(ID=df_1_2$ID)
resmodel_1_2 <- lmer(rfrontalpole ~ sex + (1|site/ParticipantID), REML = T,data = df_1_2)
res_1_2[,2] <- resid(resmodel_1_2)
names(res_1_2)[2] <- c("rfrontalpole")

res_1_2$time <- df_1_2$time
res_1_2$group <- df_1_2$group

# Calculate the mean for each time and group combination
group_means <- res_1_2 %>%
  group_by(time, group) %>%
  summarise(mean_rfrontalpole = mean(rfrontalpole, na.rm = TRUE),
            se_rfrontalpole = sd(rfrontalpole, na.rm = TRUE) / sqrt(n()),
            lower_ci_rfrontalpole = mean_rfrontalpole - (1.96* se_rfrontalpole),
            upper_ci_rfrontalpole = mean_rfrontalpole + (1.96* se_rfrontalpole))

# Plot the interaction of time by group for the rfrontalpole cortical thickness
group_means$group <- factor(group_means$group, levels = c(1, 2), labels = c("RE", "E/UE"))
group_means <- group_means %>%
  mutate(time = as.numeric(time)) %>%
  mutate(time = case_when(
    time == "1" ~ "14",
    time == "2" ~ "23"
  ))

# Plot the mean predictions with confidence intervals
ggplot(group_means, aes(x = time, y = mean_rfrontalpole, group = group)) +
  geom_line(aes(linetype = as.factor(group), color = as.factor(group)), size = 1.3) +
  geom_ribbon(aes(ymin = lower_ci_rfrontalpole, ymax = upper_ci_rfrontalpole, fill = as.factor(group), group = group), alpha = 0.2) +
  labs(title = "Trajectories of the CT in the right frontal pole",
       x = "Age",
       y = "Adjusted CT") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        text = element_text(size = 14),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.3),
        axis.title.x = element_text(size = 14)) +
  scale_linetype_manual(name = "Group", labels = c("RE", "E/UE"), values = c("RE" = "solid", "E/UE" = "solid")) +
  scale_shape_discrete(name = "Group") +
  scale_color_manual(name = "Group", labels = c("RE", "E/UE"), values = c("RE" = "#d83034", "E/UE" = "#56B4E9")) +
  scale_fill_manual(name = "Group", labels = c("RE", "E/UE"), values = c("RE" = "#d83034", "E/UE" = "#56B4E9"))

ggsave("LMM_results/figures/CT_RE_EUE.bh.pdf", units="in", width=5, height=4, dpi=600)

# There were no significant age-by-group interactions between groups 1 and 3.

# For groups2 and 3, there were 9 regions that exhibited significant age-by-group interactions. All regions showed the same directionality of their effects, so combined them into one single ROI.
res_2_3 <- data.frame(ID=df_2_3$ID)

# Calculate the sum of the specified columns for each row
df_2_3$ROI1_sum <- df_2_3 %>% 
  select(rcaudalanteriorcingulate,rcaudalmiddlefrontal,lcuneus,llingual,lpericalcarine,lrostralmiddlefrontal,rrostralmiddlefrontal,lfrontalpole,rfrontalpole) %>% 
  rowSums()

resmodel_2_3 <- lmer(ROI1_sum ~ sex + (1|site/ParticipantID), REML = T,data = df_2_3)
res_2_3[,2] <- resid(resmodel_2_3)
names(res_2_3)[2] <- c("ROI1_sum")

res_2_3$time <- df_2_3$time
res_2_3$group <- df_2_3$group

# Calculate the mean for each time and group
group_means <- res_2_3 %>%
  group_by(time, group) %>%
  summarise(mean_ROI1_sum = mean(ROI1_sum, na.rm = TRUE),
            se_ROI1_sum = sd(ROI1_sum, na.rm = TRUE) / sqrt(n()),
            lower_ci_ROI1_sum = mean_ROI1_sum - (1.96* se_ROI1_sum),
            upper_ci_ROI1_sum = mean_ROI1_sum + (1.96* se_ROI1_sum))

group_means$group <- factor(group_means$group, levels = c(2,3), labels = c("E/UE","HE"))
group_means <- group_means %>%
  mutate(time = as.numeric(time)) %>%
  mutate(time = case_when(
    time == "1" ~ "14",
    time == "2" ~ "23"
  ))

# Plotting trajectories of CT in groups 2 and 3 with confidence intervals
ggplot(group_means, aes(x = time, y = mean_ROI1_sum, group = group)) +
  geom_line(aes(linetype = as.factor(group), color = as.factor(group)), size = 1.3) +
  geom_ribbon(aes(ymin = lower_ci_ROI1_sum, ymax = upper_ci_ROI1_sum, fill = as.factor(group), group = group), alpha = 0.2) +
  labs(title = "Trajectories of the CT in the sum of 9 regions",
       x = "Age",
       y = "Adjusted CT") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        text = element_text(size = 14),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.3),
        axis.title.x = element_text(size = 14)) +
  scale_linetype_manual(name = "Group", labels = c("E/UE","HE"), values = c("E/UE" = "solid", "HE" = "solid")) +
  scale_shape_discrete(name = "Group") +
  scale_color_manual(name = "Group", labels = c("E/UE","HE"), values = c("E/UE" = "#56B4E9", "HE" = "#999999")) +
  scale_fill_manual(name = "Group", labels = c("E/UE","HE"), values = c("E/UE" = "#56B4E9", "HE" = "#999999"))

ggsave("LMM_results/figures/CT_EUE_HE.bh.pdf", units="in", width=5, height=4, dpi=600)
