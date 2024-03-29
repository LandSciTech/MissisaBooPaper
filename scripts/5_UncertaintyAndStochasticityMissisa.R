# Demonstrate uncertainty and stochasticity of demographic predictions for
# Missisa range

library(caribouMetrics)
library(tidyverse)
library(ggpubr)
theme_set(theme_bw())

dia_shp <- 23

err_col <- "grey50"

baseDir <- "."
#numbers from Johnson et al for validation
johnsonCompare <- read.csv(paste0(baseDir,"/data/Johnson et al. figures5_6.csv"))

# get outputs from demographics
missisa_dist <- read.csv(paste0(baseDir,"/data/disturbances_missisa_all.csv"))

covTableSim <- data.frame(Anthro = c(missisa_dist$Anthro, 10, 20, 30, 40, 50, 
                                     60, 70, 80, 90), 
                          Fire = unique(missisa_dist$Fire),
                          fire_excl_anthro = c(missisa_dist$fire_excl_anthro,
                                               rep(unique(missisa_dist$Fire), 9)))
covTableSim$polygon <- paste0("Missisa_", covTableSim$Anthro + 1)
covTableSim$area <- "FarNorth"
covTableSim$Total_dist <- covTableSim$Anthro + covTableSim$fire_excl_anthro

popGrowthPars <- demographicCoefficients(500,
  modelVersion = "Johnson",
  survivalModelNumber = "M1",
  recruitmentModelNumber = "M4"
)

popGrowthParsSmall <- demographicCoefficients(35,
  modelVersion = "Johnson",
  survivalModelNumber = "M1",
  recruitmentModelNumber = "M4"
)

rateSamples <- demographicRates(
  covTable = covTableSim,
  popGrowthPars = popGrowthParsSmall,
  ignorePrecision = F, returnSample = T, useQuantiles = T
)

rateSamplesLarge <- demographicRates(
  covTable = covTableSim,
  popGrowthPars = popGrowthPars,
  ignorePrecision = F, returnSample = T, useQuantiles = T
)

rateSummaries <- demographicRates(
  covTable = covTableSim, popGrowthPars = popGrowthPars,
  ignorePrecision = F, returnSample = F, useQuantiles = F
)

rateSamples$S_PIlow <- 1
rateSamples$S_PIhigh <- 1
rateSamples$rep <- as.factor(rateSamples$replicate)
levels(rateSamples$rep) <- sample(unique(rateSamples$replicate), replace = F)
rateSamples$rep <- as.character(rateSamples$rep)
rateSamplesLarge$S_PIlow <- 1
rateSamplesLarge$S_PIhigh <- 1

rateSamples$R_PIlow <- 1
rateSamples$R_PIhigh <- 1
rateSamples$fullGrp <- paste(rateSamples$rep) # ,rateSamples$Fire)
rateSamplesLarge$R_PIlow <- 1
rateSamplesLarge$R_PIhigh <- 1
rateSamplesLarge$fullGrp <- paste(rateSamplesLarge$rep) # ,rateSamplesLarge$Fire)

str(johnsonCompare)
johnsonCompare$Anthro=johnsonCompare$anthro
johnsonCompare$S_bar=johnsonCompare$Sresp
johnsonCompare$S_PIlow = johnsonCompare$Slow_95_pred
johnsonCompare$S_PIhigh = johnsonCompare$Shigh_95_pred
johnsonCompare$R_bar=johnsonCompare$Rresp/100
johnsonCompare$R_PIlow = johnsonCompare$Rlow_95_pre/100
johnsonCompare$R_PIhigh = johnsonCompare$Rhigh_95_pred/100


base1 <- ggplot(data = rateSummaries, 
                aes(x = Anthro, y = S_bar, ymin = S_PIlow, ymax = S_PIhigh)) +
  geom_ribbon(fill="grey95",colour="grey95",data=johnsonCompare)+
  geom_line(data = subset(rateSamples), size = 0.5, alpha = 0.5, 
            aes(x = Anthro, y = S_bar, group = rep, colour = rep)) +
  geom_line(colour = "grey50", size = 2, linetype = "dotted") +
  geom_line(colour = "black", size = 2, linetype = "dotted",
            data = subset(johnsonCompare, Anthro <= 90)) +
  # geom_boxplot(data=subset(rateSamplesLarge,Anthro<10),aes(x=Anthro,y=S_bar,group=Anthro),width=2)+
  geom_errorbar(data = subset(rateSummaries, Anthro < 10), 
                width = 2, size = 0.7, col = err_col) +
  # geom_point(data=subset(rateSummaries,Anthro<10))+
  geom_point(data = data.frame(Anthro = 0.27, S_bar = 0.8, 
                               S_PIlow = 0, S_PIhigh = 0), 
             size = 2, shape = dia_shp, fill = "black") +
  xlab("Anthropogenic Disturbance (%)") +
  ylab("Adult Female Survival") +
  scale_x_continuous(limits = c(-1, 90), breaks = c(0, 20, 40, 60, 80)) +
  scale_y_continuous(limits = c(0.65, 1)) +
  theme(legend.position = "none", plot.margin = margin(l = 0.6, unit = "cm"))

plot_recruitment3 <- ggplot(data = rateSummaries,
                            aes(x = Anthro, y = R_bar * 100, 
                                ymin = R_PIlow * 100, ymax = R_PIhigh * 100)) +
  geom_ribbon(fill="grey95",colour="grey95",data=johnsonCompare)+
  geom_line(data = rateSamples, size = 0.5, 
            aes(x = Anthro, y = R_bar * 100, group = fullGrp, color = fullGrp),
            alpha = 0.5) +
  geom_line(colour = "grey50", size = 2, linetype = "dotted") +
  geom_line(colour = "black", size = 2, linetype = "dotted",
            data = subset(johnsonCompare, Anthro <= 90)) +
  # geom_boxplot(data=subset(rateSamplesLarge,Anthro<10),aes(x=Anthro,y=R_bar*100,group=Anthro),width=2)+
  geom_errorbar(data = subset(rateSummaries, Anthro < 10), width = 2,
                size = 0.7, col = err_col) +
  # geom_point(data=subset(rateSummaries,Anthro<10))+
  geom_point(data = data.frame(Anthro = 0.27, R_bar = 0.142, R_PIlow = 0, 
                               R_PIhigh = 0),
             size = 2, shape = dia_shp, fill = "black") +
  scale_x_continuous(limits = c(-1, 90), breaks = c(0, 20, 40, 60, 80)) +
  scale_y_continuous(limits = c(0, 60), breaks = c(0, 10, 20, 30, 40, 50, 60)) +
  xlab("Anthropogenic disturbance (%)") +
  ylab("Recruitment (calves/100 cows)") +
  theme(legend.position = "none", plot.margin = margin(l = 0.6, unit = "cm"))

# demography
pars <- data.frame(N0 = c(round(745 / 2)))
# increase to get a better sample size, or set interannualVar to NA
pars <- merge(pars, data.frame(rrp = 1))
pars <- merge(pars, rateSamplesLarge)
numSteps <- 20
pars1 <- cbind(pars, caribouPopGrowth(pars$N0,
  numSteps = numSteps, R_bar = pars$R_bar,
  S_bar = pars$S_bar, probOption = "binomial"
))

pars <- data.frame(N0 = round(745 / 2))
# increase to get a better sample size, or set interannualVar to NA
pars <- merge(pars, data.frame(rrp = 1)) 
pars <- merge(pars, rateSamples)
numSteps <- 20
pars2 <- cbind(pars, caribouPopGrowth(pars$N0,
  numSteps = numSteps, R_bar = pars$R_bar,
  S_bar = pars$S_bar, probOption = "binomial"
))


check <- subset(pars1, Anthro == missisa_dist$Anthro[3])
testPars <- list(R_bar = mean(check$R_bar), S_bar = mean(check$S_bar))
caribouPopGrowth(round(745 / 2), numSteps = 20, R_bar = testPars$R_bar, 
                 S_bar = testPars$S_bar, probOption = "continuous", 
                 interannualVar = F)
# this is theoretical lambda - to confirm
(testPars$S_bar) * (1 + testPars$R_bar * 0.5) 
mean(check$lambda)
testPars
# The math seems ok here.

oo <- pars2 %>%
  select(Anthro, lambda, fullGrp, rrp) %>%
  group_by(fullGrp, Anthro) %>%
  summarise(lambda = median(lambda))

subset(oo, Anthro == missisa_dist$Anthro[1])
ooT <- pars1 %>%
  select(Anthro, lambda, fullGrp, rrp) %>%
  group_by(fullGrp, Anthro) %>%
  summarise(lambda = median(lambda))

ooS <- ooT %>%
  select(Anthro, lambda) %>%
  group_by(Anthro) %>%
  summarise(lambdaH = max(lambda), lambdaL = min(lambda), lambda = median(lambda))

oo$lambdaH <- oo$lambda
oo$lambdaL <- oo$lambda
str(ooS)
plot_lambda <- ggplot(oo, 
                      aes(x = Anthro, y = lambda, ymin = lambdaH, ymax = lambdaL)) +
  geom_line(size = 0.5,
            aes(x = Anthro, y = lambda, group = fullGrp, color = fullGrp), 
            alpha = 0.5) +
  geom_errorbar(data = subset(ooS, Anthro < 10), width = 2, 
                size = 0.7, col = err_col) +
  geom_point(data = data.frame(Anthro = 0.27, lambda = 0.86, fullGrp = 0,
                               lambdaH = 0.86, lambdaL = 0.86), 
             size = 2, shape = dia_shp, fill = "black") +
  scale_x_continuous(limits = c(-1, 90), breaks = c(0, 20, 40, 60, 80)) +
  xlab("Anthropogenic disturbance (%)") +
  ylab(expression("Average Growth Rate " * lambda)) +
  theme(legend.position = "none", plot.margin = margin(l = 0.6, unit = "cm"))

# combine ggplots to one figure
ggpubr::ggarrange(base1, plot_recruitment3, plot_lambda, labels = "auto",
                  ncol = 1, vjust = 1)

ggsave(paste0(baseDir,"/outputs/Figure5_missisaDemoRates.tiff"), width = 5.1, height = 7, units = "in", 
       dpi = 300)
