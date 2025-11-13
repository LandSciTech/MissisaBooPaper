# Demonstrate uncertainty and stochasticity of demographic predictions for
# Missisa range

library(caribouMetrics)
library(tidyverse)
library(ggpubr)
theme_set(theme_bw())

dia_shp <- 23

err_col <- "grey50"

baseDir <- "."
#baseDir <- "C:/Users/HughesJo/Documents/gitprojects/MissisaBooPaper"
#numbers from Johnson et al for validation
johnsonCompare <- read.csv(paste0(baseDir,"/data/Johnson et al. figures5_6.csv"))

# get outputs from demographics
missisa_dist <- read.csv(paste0(baseDir,"/data/disturbances_missisa_all.csv"))

disturbanceChange <- data.frame(Anthro = seq(1,90,by=2), 
                          Fire = unique(missisa_dist$Fire),
                          fire_excl_anthro = unique(missisa_dist$Fire))
#covTableSim$polygon <- paste0("Missisa_", covTableSim$Anthro + 1)
#covTableSim$area <- "FarNorth"

disturbanceChange$Total_dist <- disturbanceChange$Anthro + disturbanceChange$fire_excl_anthro
disturbanceChange$Year <- seq(1:nrow(disturbanceChange))

N0 = c(round(745 / 2))

#devtools::document();devtools::load_all()

sampleTrajectories <- trajectoriesFromNational(replicates = 25, N0 = N0,
                                               useQuantiles  = T,
                                               disturbance = disturbanceChange,
                                               doSummary=T)
sampleTrajectories$samples$lower=1
sampleTrajectories$samples$upper=1

unique(sampleTrajectories$samples$MetricTypeID)

changeSummary <- trajectoriesFromNational(replicates=500,N0=N0,useQuantiles=F,returnSamples=F,disturbance=disturbanceChange)
missisaSummary <- trajectoriesFromNational(replicates=500,N0=N0,useQuantiles=F,disturbance=missisa_dist)

str(johnsonCompare)
johnsonCompare$R_bar=johnsonCompare$Rresp/100
johnsonCompare$R_PIlow = johnsonCompare$Rlow_95_pre/100
johnsonCompare$R_PIhigh = johnsonCompare$Rhigh_95_pred/100

str(johnsonCompare)
johnsonSurv <- subset(johnsonCompare,select=c(anthro,Sresp,Slow_95_pred,Shigh_95_pred))
names(johnsonSurv)<- c("AnthroID","Mean","lower","upper")
johnsonRec <- subset(johnsonCompare,select=c(anthro,R_bar,R_PIlow,R_PIhigh))
names(johnsonRec)<- c("AnthroID","Mean","lower","upper")


str(changeSummary$summary)
unique(changeSummary$summary$Parameter)
cPar= "Expected survival";cMetric="Sbar"
base1 <- ggplot(data = subset(changeSummary$summary,Parameter==cPar), 
                aes(x = AnthroID, y = Mean, ymin = lower, ymax = upper)) +
  geom_ribbon(fill="grey95",colour="grey95",data=johnsonSurv)+
  geom_line(data = subset(sampleTrajectories$samples,MetricTypeID==cMetric), size = 0.5, alpha = 0.25, 
            aes(x = AnthroID, y = Amount, group = Replicate, colour = Replicate)) +
  geom_line(colour = "grey50", size = 2, linetype = "dotted") +
  geom_line(colour = "black", size = 2, linetype = "dotted",
            data = subset(johnsonSurv, AnthroID <= 90)) +
  # geom_boxplot(data=subset(rateSamplesLarge,Anthro<10),aes(x=Anthro,y=S_bar,group=Anthro),width=2)+
  geom_errorbar(data = subset(missisaSummary$summary, Parameter==cPar), 
                width = 2, size = 0.7, col = err_col) +
  # geom_point(data=subset(rateSummaries,Anthro<10))+
  geom_point(data = data.frame(AnthroID = 0.27, Mean = 0.8, 
                               lower = 0, upper = 0), 
             size = 2, shape = dia_shp, fill = "black") +
  xlab("Anthropogenic Disturbance (%)") +
  ylab(cPar) +
  scale_x_continuous(limits = c(-1, 90), breaks = c(0, 20, 40, 60, 80)) +
  scale_y_continuous(limits = c(0.65, 1)) +
  theme(legend.position = "none", plot.margin = margin(l = 0.6, unit = "cm"))
plot(base1)

cPar= "Expected recruitment";cMetric="Rbar"
plot_recruitment3 <- ggplot(data = subset(changeSummary$summary,Parameter==cPar), 
                aes(x = AnthroID, y = Mean*100, ymin = lower*100, ymax = upper*100)) +
  geom_ribbon(fill="grey95",colour="grey95",data=johnsonRec)+
  geom_line(data = subset(sampleTrajectories$samples,MetricTypeID==cMetric), size = 0.5, alpha = 0.25, 
            aes(x = AnthroID, y = Amount*100, group = Replicate, colour = Replicate)) +
  geom_line(colour = "grey50", size = 2, linetype = "dotted") +
  geom_line(colour = "black", size = 2, linetype = "dotted",
            data = subset(johnsonRec, AnthroID <= 90)) +
  # geom_boxplot(data=subset(rateSamplesLarge,Anthro<10),aes(x=Anthro,y=S_bar,group=Anthro),width=2)+
  geom_errorbar(data = subset(missisaSummary$summary, Parameter==cPar), 
                width = 2, size = 0.7, col = err_col) +
  # geom_point(data=subset(rateSummaries,Anthro<10))+
  geom_point(data = data.frame(AnthroID = 0.27, Mean = 0.142, lower = 0, 
                               upper = 0),
             size = 2, shape = dia_shp, fill = "black") +
  xlab("Anthropogenic Disturbance (%)") +
  ylab(cPar) +
  scale_x_continuous(limits = c(-1, 90), breaks = c(0, 20, 40, 60, 80)) +
  scale_y_continuous(limits = c(0, 60), breaks = c(0, 10, 20, 30, 40, 50, 60)) +
  theme(legend.position = "none", plot.margin = margin(l = 0.6, unit = "cm"))
plot(plot_recruitment3)

cPar= "Expected growth rate";cMetric="lambda_bar"
plot_lambda <- ggplot(data = subset(changeSummary$summary,Parameter==cPar), 
                            aes(x = AnthroID, y = Mean, ymin = lower, ymax = upper)) +
  geom_line(data = subset(sampleTrajectories$samples,MetricTypeID==cMetric), size = 0.5, alpha = 0.25, 
            aes(x = AnthroID, y = Amount, group = Replicate, colour = Replicate)) +
  geom_line(colour = "grey50", size = 2, linetype = "dotted") +
  # geom_boxplot(data=subset(rateSamplesLarge,Anthro<10),aes(x=Anthro,y=S_bar,group=Anthro),width=2)+
  geom_errorbar(data = subset(missisaSummary$summary, Parameter==cPar), 
                width = 2, size = 0.7, col = err_col) +
  # geom_point(data=subset(rateSummaries,Anthro<10))+
  geom_point(data = data.frame(AnthroID = 0.27, Mean = 0.86, lower = 0, 
                               upper = 0),
             size = 2, shape = dia_shp, fill = "black") +
  xlab("Anthropogenic Disturbance (%)") +
  ylab(expression("Expected Growth Rate " * bar(lambda))) +
  scale_x_continuous(limits = c(-1, 90), breaks = c(0, 20, 40, 60, 80)) +
  scale_y_continuous(limits = c(0.5, 1.5)) +
  theme(legend.position = "none", plot.margin = margin(l = 0.6, unit = "cm"))
plot(plot_lambda)

cPar= "Population growth rate";cMetric="lambda"
plot_lambdaT <- ggplot(data = subset(changeSummary$summary,Parameter==cPar), 
                      aes(x = AnthroID, y = Mean, ymin = lower, ymax = upper)) +
  geom_line(data = subset(sampleTrajectories$samples,MetricTypeID==cMetric), size = 0.5, alpha = 0.25, 
            aes(x = AnthroID, y = Amount, group = Replicate, colour = Replicate)) +
  #geom_line(colour = "grey50", size = 2, linetype = "dotted") +
  # geom_boxplot(data=subset(rateSamplesLarge,Anthro<10),aes(x=Anthro,y=S_bar,group=Anthro),width=2)+
  geom_errorbar(data = subset(missisaSummary$summary, Parameter==cPar), 
                width = 2, size = 0.7, col = err_col) +
  # geom_point(data=subset(rateSummaries,Anthro<10))+
  geom_point(data = data.frame(AnthroID = 0.27, Mean = 0.86, lower = 0, 
                               upper = 0),
             size = 2, shape = dia_shp, fill = "black") +
  xlab("Anthropogenic Disturbance (%)") +
  ylab(expression("Annual Growth Rate " * lambda)) +
  scale_x_continuous(limits = c(-1, 90), breaks = c(0, 20, 40, 60, 80)) +
  scale_y_continuous(limits = c(0.5, 1.5)) +
  theme(legend.position = "none", plot.margin = margin(l = 0.6, unit = "cm"))
plot(plot_lambdaT)


# combine ggplots to one figure
ggpubr::ggarrange(base1, plot_recruitment3, plot_lambda,plot_lambdaT, labels = "auto",
                  ncol = 1, vjust = 1)
ggsave(paste0(baseDir,"/outputs/Figure5_missisaDemoRates.tiff"), width = 5.1, height = 7, units = "in", 
       dpi = 300)
ggsave(paste0(baseDir,"/outputs/Figure5_missisaDemoRates.png"), width = 5.1, height = 7, units = "in", 
       dpi = 300)
ggsave(paste0(baseDir,"/outputs/Figure5_missisaDemoRates.pdf"), width = 5.1, height = 7, units = "in")

