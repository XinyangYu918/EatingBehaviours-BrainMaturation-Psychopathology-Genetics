#### Perform multivariate latent growth curve modelling in each group ####
library(psych)
library(lavaan)
library(ggplot2)

# Import data
internalizing <- read.csv("./for_sem.csv")
internalizing.c1 <- subset(internalizing, cluster == "1")
internalizing.c2 <- subset(internalizing, cluster == "2")
internalizing.c3 <- subset(internalizing, cluster == "3")

# For cluster1
attach(internalizing.c1)

# Specify linear intercept and slope model
LIN <- "
ip_int =~ 1*ip1 + 1*ip2 + 1*ip3 + 1*ip4
ip_slp =~ 0*ip1 + 2*ip2 + 5*ip3 + 9*ip4

ep_int =~ 1*ep1 + 1*ep2 + 1*ep3 + 1*ep4
ep_slp =~ 0*ep1 + 2*ep2 + 5*ep3 + 9*ep4
        
ip_int ~ gender + s1 + s2 + s3 + s4 + s5 + s6 + s7
ip_slp ~ gender + s1 + s2 + s3 + s4 + s5 + s6 + s7
ep_int ~ gender + s1 + s2 + s3 + s4 + s5 + s6 + s7
ep_slp ~ gender + s1 + s2 + s3 + s4 + s5 + s6 + s7

ip1~~ep1
ip2~~ep2
ip3~~ep3
ip4~~ep4"

# Examine fit indices and standardized solution, and save results
LIN.fit <- growth(model = LIN, data = internalizing.c1, missing = "ML")
summary(LIN.fit, fit.measures = T, standardized = T, rsquare = T, ci = T)
capture.output( summary(LIN.fit, fit.measures = T, standardized = T, rsquare = T, ci = T), file = './multivariate_LGCM/results/IP_EP_cluster1.fit.txt')

# For cluster2
attach(internalizing.c2)

# Specify linear intercept and slope model
LIN <- "
ip_int =~ 1*ip1 + 1*ip2 + 1*ip3 + 1*ip4
ip_slp =~ 0*ip1 + 2*ip2 + 5*ip3 + 9*ip4

ep_int =~ 1*ep1 + 1*ep2 + 1*ep3 + 1*ep4
ep_slp =~ 0*ep1 + 2*ep2 + 5*ep3 + 9*ep4
        
ip_int ~ gender + s1 + s2 + s3 + s4 + s5 + s6 + s7
ip_slp ~ gender + s1 + s2 + s3 + s4 + s5 + s6 + s7
ep_int ~ gender + s1 + s2 + s3 + s4 + s5 + s6 + s7
ep_slp ~ gender + s1 + s2 + s3 + s4 + s5 + s6 + s7

ip1~~ep1
ip2~~ep2
ip3~~ep3
ip4~~ep4"

# Examine fit indices and standardized solution, and save results
LIN.fit <- growth(model = LIN, data = internalizing.c2, missing = "ML")
summary(LIN.fit, fit.measures = T, standardized = T, rsquare = T, ci = T)
capture.output( summary(LIN.fit, fit.measures = T, standardized = T, rsquare = T, ci = T), file = './multivariate_LGCM/results/IP_EP_cluster2.fit.txt')

# For cluster3
attach(internalizing.c3)

# Specify linear intercept and slope model
LIN <- "
ip_int =~ 1*ip1 + 1*ip2 + 1*ip3 + 1*ip4
ip_slp =~ 0*ip1 + 2*ip2 + 5*ip3 + 9*ip4

ep_int =~ 1*ep1 + 1*ep2 + 1*ep3 + 1*ep4
ep_slp =~ 0*ep1 + 2*ep2 + 5*ep3 + 9*ep4
        
ip_int ~ gender + s1 + s2 + s3 + s4 + s5 + s6 + s7
ip_slp ~ gender + s1 + s2 + s3 + s4 + s5 + s6 + s7
ep_int ~ gender + s1 + s2 + s3 + s4 + s5 + s6 + s7
ep_slp ~ gender + s1 + s2 + s3 + s4 + s5 + s6 + s7

ip1~~ep1
ip2~~ep2
ip3~~ep3
ip4~~ep4"

# Examine fit indices and standardized solution, and save results
LIN.fit <- growth(model = LIN, data = internalizing.c3, missing = "ML")
summary(LIN.fit, fit.measures = T, standardized = T, rsquare = T, ci = T)
capture.output( summary(LIN.fit, fit.measures = T, standardized = T, rsquare = T, ci = T), file = './multivariate_LGCM/results/IP_EP_cluster3.fit.txt')
