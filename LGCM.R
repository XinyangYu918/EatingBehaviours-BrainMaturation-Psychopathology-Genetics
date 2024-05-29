#### Perform univariate LGCM for internalising and externalising problems ####
library(psych)
library(ggplot2)
library(lavaan)
library(dplyr)
library(tidyr)
library(ggpubr)

# Load data
internalizing <- read.csv("./for_sem.csv")

# For all three group
attach(internalizing)

LIN <- "ip_int =~ 1*ip1 + 1*ip2 + 1*ip3 + 1*ip4
        ip_slp =~ 0*ip1 + 2*ip2 + 5*ip3 + 9*ip4
        
ip_int ~ gender + cluster1 + cluster2 + s1 + s2 + s3 + s4 + s5 + s6 + s7
ip_slp ~ gender + cluster1 + cluster2 + s1 + s2 + s3 + s4 + s5 + s6 + s7 "

LIN.fit <- growth(model = LIN, data = internalizing, missing = "ML")
summary(LIN.fit, fit.measures = T, standardized = T, rsquare = T, ci = T)
capture.output(summary(LIN.fit, fit.measures = T, standardized = T, rsquare = T, ci = T), file = './LGCM/results/all/LIN_ip.fit.txt')

# Obtain estimates of trajectories for each individual
predicted.df <- lavPredict(LIN.fit,type = "lv", method = "EBM", label = "T")
predicted.df2 <- data.frame(PSC2, cluster,predicted.df, ip1, ip2, ip3, ip4)
write.csv(predicted.df2, file = './LGCM/results/all/pred_ip.csv', na = "", row.names = F)

# Plot predicted trajectories of three groups
data <- read.csv("./LGCM/results/all/pred_ip.csv")
data$pred_ip1 <- data$ip_int + 0*data$ip_slp
data$pred_ip2 <- data$ip_int + 2*data$ip_slp
data$pred_ip3 <- data$ip_int + 5*data$ip_slp
data$pred_ip4 <- data$ip_int + 9*data$ip_slp

# Transform to a long format
data_long <- data %>%
  pivot_longer(cols = c(pred_ip1, pred_ip2, pred_ip3, pred_ip4), names_to = "time_point", values_to = "pred_ip") %>%
  mutate(time_point = case_when(
    time_point == "pred_ip1" ~ "14",
    time_point == "pred_ip2" ~ "16",
    time_point == "pred_ip3" ~ "19",
    time_point == "pred_ip4" ~ "23"
  )) %>%
  mutate(time_point = as.numeric(time_point))

# Get mean and SE for each cluster at each time point
mean_pred_ip <- data_long %>%
  group_by(cluster, time_point) %>%
  summarize(
    mean = mean(pred_ip, na.rm = TRUE),
    se = sd(pred_ip, na.rm = TRUE) / sqrt(n()),
    lower_ci = mean - (1.96 * se),  # Lower bound of the 95% CI
    upper_ci = mean + (1.96 * se)   # Upper bound of the 95% CI
  )
names(mean_pred_ip)[1] <- "Group"
mean_pred_ip$Group[mean_pred_ip$Group=="1"] <- "RE"
mean_pred_ip$Group[mean_pred_ip$Group=="2"] <- "E/UE"
mean_pred_ip$Group[mean_pred_ip$Group=="3"] <- "HE"

# Plot and save the figure
p <- ggplot(mean_pred_ip, aes(x = time_point, y = mean, group = Group)) +
  geom_line(aes(color = as.factor(Group))) +  # Apply color aesthetic to lines
  geom_point(aes(color = as.factor(Group))) +  # Apply color to points
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, fill = as.factor(Group)), alpha = 0.2) +  # Apply fill color to ribbons
  labs(title = "Predicted IP trajectories in three groups",
       x = "Age",
       y = "IP scores") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    text = element_text(size = 14),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 14)) +
  scale_color_manual(name = "Group", labels = c("E/UE","HE","RE"), values = c("#56B4E9", "#999999","#d83034")) +  
  scale_fill_manual(name = "Group", labels = c( "E/UE","HE","RE"), values = c("#56B4E9", "#999999","#d83034")) +  
  scale_x_continuous(breaks = c(14, 16, 19, 23), labels = c("14", "16", "19", "23"), limits = c(14, 23)) 

ggsave("predicted_IP_trajectories.pdf", plot = p, width = 8, height = 6, dpi = 600)

# For clusters 1 and 2 only
internalizing.c12 <- subset(internalizing, cluster %in% c(1,2))
attach(internalizing.c12)
LIN2 <- "ip_int =~ 1*ip1 + 1*ip2 + 1*ip3 + 1*ip4
        ip_slp =~ 0*ip1 + 2*ip2 + 5*ip3 + 9*ip4
        
ip_int ~ gender + cluster1 + s1 + s2 + s3 + s4 + s5 + s6 + s7
ip_slp ~ gender + cluster1 + s1 + s2 + s3 + s4 + s5 + s6 + s7"

LIN2.fit <- growth(model = LIN2, data = internalizing.c12, missing = "ML")
summary(LIN2.fit, fit.measures = T, standardized = T, rsquare = T, ci = T)
capture.output(summary(LIN2.fit, fit.measures = T, standardized = T, rsquare = T, ci = T), file = './LGCM/results/clusters12/LIN_ip.fit.txt')

predicted.df <- lavPredict(LIN2.fit,type = "lv", method = "EBM", label = "T")
predicted.df2 <- data.frame(PSC2, cluster,predicted.df, ip1, ip2, ip3, ip4)
write.csv(predicted.df2, file = './LGCM/results/clusters12/pred_ip.csv', na = "", row.names = F)
