# Sensitivity to N0 - manuscript appendix A
library(caribouMetrics)

library(tidyverse)
theme_set(theme_bw())
#################
# Get population recruitment and survival samples, and associated population
# projections
popGrowthPars <- demographicCoefficients(
  100, modelVersion = "Johnson",
  survivalModelNumber = "M1",
  recruitmentModelNumber = "M4"
)

covTableSim <- expand.grid(Anthro = seq(0, 80, by = 20), 
                           fire_excl_anthro = seq(0, 60, by = 20))
covTableSim$polygon <- "test"
covTableSim$Total_dist <- covTableSim$Anthro + covTableSim$fire_excl_anthro
covTableSim <- subset(covTableSim, Total_dist <= 100)

###########
# Is the relationship between rates and lambda correct?
rateSamplesP <- demographicRates(
  covTable = covTableSim,
  popGrowthPars = popGrowthPars,
  ignorePrecision = F, returnSample = T, useQuantiles = F
)

# project each sample population
pars <- data.frame(N0 = c(10, 20, 30, 40, 50, 60, 70))
# increase to get a better sample size, or set interannualVar to NA
pars <- merge(pars, data.frame(rrp = seq(1, 1))) 
pars <- merge(pars, rateSamplesP)

nrow(pars)
numSteps <- 20
pars1 <- cbind(pars, popGrowthJohnson(pars$N0,
  numSteps = numSteps, R_bar = pars$R_bar,
  S_bar = pars$S_bar, probOption = "binomial"
))

sum(pars1$N == 0)
# pars1=pars
pars2 <- cbind(pars, popGrowthJohnson(pars$N0,
  numSteps = numSteps, R_bar = pars$R_bar,
  S_bar = pars$S_bar, probOption = "matchJohnson2020"
))
min(pars2$N)

pars3 <- cbind(pars, popGrowthJohnson(pars$N0,
  numSteps = numSteps, R_bar = pars$R_bar,
  S_bar = pars$S_bar, probOption = "continuous"
))
min(pars3$N)


####################
# Compare
pars1$method <- "binomial"
pars2$method <- "rounding"
pars3$method <- "continuous"
see <- rbind(pars1, pars2, pars3)
see$grp <- paste0(see$method, see$N0)
see$fireExclAnthro <- as.factor(see$fire_excl_anthro)
raw <- ggplot(see, aes(x = N0, y = lambda, colour = method, group = grp)) +
  facet_grid(Anthro ~ fireExclAnthro, 
             labeller = labeller(Anthro = label_both,
                                 fireExclAnthro = label_both)) +
  geom_violin(position = "dodge", scale = "width", adjust = 0.5) +
  theme(legend.position = "top") +
  ylab(expression("Average Population Trend " * lambda)) +
  xlab("Initial Population Size")

ggsave("outputs/WholeNumberMethodLambda.tiff", raw, width = 7, height = 7, dpi = 300)

see$Na <- see$N
see$Na[see$N == 0] <- 0.001
finalN <- ggplot(see, aes(x = N0, y = Na, colour = method, group = grp)) +
  facet_grid(Anthro ~ fireExclAnthro, 
             labeller = labeller(Anthro = label_both, 
                                 fireExclAnthro = label_both)) +
  geom_violin(position = "dodge", scale = "width", adjust = 0.1) +
  theme(legend.position = "top") +
  scale_y_continuous(trans = "log10", breaks = c(0.001, 0.01, 1, 100), 
                     labels = c(0, 0.1, 1, 100)) +
  ylab("Population Size at t=20") +
  xlab("Initial Population Size")
ggsave("outputs/WholeNumberMethodN.tiff", finalN, width = 7, height = 7, dpi = 300)
