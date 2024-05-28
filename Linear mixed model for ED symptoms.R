library(dplyr)
library(lme4)
library(lmerTest)
library(tidyr)
library(readr)
library(ggplot2)
library(emmeans)

dieting <- read.csv("DAWBA_ED/dieting.csv")
table(dieting$dieting1)
table(dieting$dieting2)
table(dieting$dieting3)
table(dieting$dieting4)
dieting$group <- as.factor(dieting$group)

dieting_long <- dieting %>%
  pivot_longer(
    cols = starts_with("dieting"),
    names_to = "time",
    values_to = "dieting") %>%
  mutate(time = parse_number(time)) 

dieting_long$subject <- rep(1:996, each = 4)
dieting_long$time <- as.factor(dieting_long$time)
dieting_long$group <- relevel(dieting_long$group, ref = "3")
dieting_long$group <- as.factor(dieting_long$group)

# Run linear mixed models
lm.model <- lmer(dieting ~ time * group + sex + (1|site/subject), data = dieting_long, na.action = na.exclude)
summary(lm.model)

# Post-hoc tests
interaction_emmeans <- emmeans(lm.model, ~ group | time)
pairs(interaction_emmeans, adjust = "bonferroni")

# Draw trajectories of dieting symptoms
res.model <- lmer(dieting ~ sex + (1|site/subject), data = dieting_long, na.action = na.exclude)
dieting_long$residuals <- resid(res.model)

group_means <- dieting_long %>%
  group_by(time, group) %>%
  summarise(mean = mean(residuals, na.rm = TRUE),
            se = sd(residuals, na.rm = TRUE) / sqrt(n()),
            lower_ci = mean - (1.96* se),
            upper_ci = mean + (1.96* se))

group_means$group <- factor(group_means$group, levels = c(1,2,3), labels = c("RE","E/UE","HE"))
group_means <- group_means %>%
  mutate(time = as.numeric(time)) %>%
  mutate(time = case_when(
    time == "1" ~ "14",
    time == "2" ~ "16",
    time == "3" ~ "19",
    time == "4" ~ "23"
  ))

group_means$time <- as.factor(group_means$time)

p <- ggplot(group_means, aes(x = time, y = mean, group = group)) +
  geom_line(aes(linetype = as.factor(group), color = as.factor(group)), size = 1.3) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, 
                  fill = as.factor(group), group = group), alpha = 0.2) +
  theme_minimal() +
  labs(x = "Age", y = "Adjusted dieting symptoms
       ", title = "Trajectories of dieting symptoms
       ") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        text = element_text(size = 14),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.3),
        axis.title.x = element_text(size = 14)) +
  scale_linetype_manual(name = "Group", labels = c("RE","E/UE","HE"), 
                        values = c("RE" = "solid","E/UE" = "solid", "HE" = "solid")) +
  scale_shape_discrete(name = "Group") +
  scale_color_manual(name = "Group", labels = c("RE","E/UE","HE"), 
                     values = c("RE" = "#d83034", "E/UE" = "#56B4E9", "HE" = "#999999")) +
  scale_fill_manual(name = "Group", labels = c("RE","E/UE","HE"), 
                    values = c("RE" = "#d83034", "E/UE" = "#56B4E9", "HE" = "#999999"))

ggsave("dieting trajectories.pdf", plot = p, device = "pdf", dpi = 600)
