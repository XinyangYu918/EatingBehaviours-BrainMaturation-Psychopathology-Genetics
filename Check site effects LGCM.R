#### Check effects of recruitment sites on the estimation of internalising or externalising problems ####
library(lavaan)

# Load data, for internalising problems
internalizing <- read.csv("./for_sem.csv")
attach(internalizing)

# Fit the free model, allowing the coefficients to vary 
multigroup <- "
ip_int =~ 1*ip1 + 1*ip2 + 1*ip3 + 1*ip4
ip_slp =~ 0*ip1 + 2*ip2 + 5*ip3 + 9*ip4
ip_int ~ gender
ip_slp ~ gender"

multigroup.free <- growth(model = multigroup, data = internalizing, missing = "ML", group = "site")
summary(multigroup.free, fit.measures = T, standardized = T, rsquare = T, ci = T)
capture.output(summary(multigroup.free, fit.measures = T, standardized = T, rsquare = T, ci = T), file = './study sites/ip/multigroup.free.fit.txt')

# Fit the constrained model
multigroup.constrained <- growth(model = multigroup, data = internalizing, missing = "ML", group = "site", group.equal = c("intercepts", "regressions"))
summary(multigroup.constrained, fit.measures = T, standardized = T, rsquare = T, ci = T)
capture.output(summary(multigroup.constrained, fit.measures = T, standardized = T, rsquare = T, ci = T), file = './study sites/ip/multigroup.constrained.fit.txt')

# Compare the two models using a Chi-squared difference test
anova(multigroup.free, multigroup.constrained)
capture.output(anova(multigroup.free, multigroup.constrained), file = './study sites/ip/multigroup.comparisons.txt')

# Load data, for externalising problems
externalizing <- read.csv("./for_sem2.csv")
attach(externalizing)

# Fit the free model, allowing the coefficients to vary 
multigroup <- "
ep_int =~ 1*ep1 + 1*ep2 + 1*ep3 + 1*ep4
ep_slp =~ 0*ep1 + 2*ep2 + 5*ep3 + 9*ep4
ep_int ~ gender
ep_slp ~ gender"

multigroup.free <- growth(model = multigroup, data = externalizing, missing = "ML", group = "site")
summary(multigroup.free, fit.measures = T, standardized = T, rsquare = T, ci = T)
capture.output(summary(multigroup.free, fit.measures = T, standardized = T, rsquare = T, ci = T), file = './study sites/ep/multigroup.free.fit.txt')

multigroup.constrained <- growth(model = multigroup, data = externalizing, missing = "ML", group = "site", group.equal = c("intercepts", "regressions"))
summary(multigroup.constrained, fit.measures = T, standardized = T, rsquare = T, ci = T)
capture.output(summary(multigroup.constrained, fit.measures = T, standardized = T, rsquare = T, ci = T), file = './study sites/ep/multigroup.constrained.fit.txt')

anova(multigroup.free, multigroup.constrained)
capture.output(anova(multigroup.free, multigroup.constrained), file = './study sites/ep/multigroup.comparisons.txt')
