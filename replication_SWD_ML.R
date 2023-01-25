########################################################################################
######### An Empirical Evaluation of Explanations for Political System Support #########
########################################################################################

library(haven)
# install.packages("mlr")
library(mlr)
# install.packages("mlbench")
library(mlbench)
# install.packages("kernlab")
library(kernlab)
# install.packages("ranger")
library(ranger)
# install.packages("cmaes")
library(cmaes)
# install.packages("mmpf")
library(mmpf)
# install.packages("stargazer")
library(stargazer)
# install.packages("tidyverse")
library(tidyverse)
# install.packages("devtools")
library(devtools)
# install_github("mikajoh/wiggle")
library(wiggle)
# install.packages("QuantPsyc")
library(QuantPsyc)
library(lubridate)
library(fuzzyjoin)


ESS6 <- read.csv("ESS6_cleaned_new.csv") 

#### LOOP FOR VAR. IMPORTANCE ####
system.time({
  
  #PREAMBLE LOOP  
  cntry <- as.character(unique(ESS6$cntry))
  regr.task <- list()
  mod <- list()
  lrn <- list()
  importance <- list()
  set.seed(2021)
  lrn <- makeLearner("regr.ranger", importance = c("permutation"))
  
  ## START LOOP
  for (i in 1: length(cntry)){
    data <- ESS6
    data <- data[data$cntry == cntry[i], ]
    
    
    
    # DEFINE TASK 
    regr.task[[i]] <-  makeRegrTask(data = data, target = "stfdem")
    
    # FEATURE IMPORTANCE 
    mod[[i]] <- train(lrn, regr.task[[i]])
    importance[[i]] <- getFeatureImportance(mod[[i]], nmc = 50L, interaction = TRUE, local = TRUE)
    importance[[i]] <- gather(importance[[i]]$res, "var.name", "importance")
    # importance[[i]] <- rename(importance[[i]][,1:2], c("importance" = cntry[i]))
  } ## END LOOP
  
  # MAKE IMPORTANCE DF
  importance2 <- data.frame(importance[1])
  for (i in 2:length(cntry)){
    importance2[,i+1] <- importance[[i]]$importance
  } ## END LOOP
  
  #POST
  colnames(importance2) <- c("var.name", cntry)
  
  importance2 <- importance2[-1,]
}) # End system.time

#### PLOT IMPORTANCE #####
imp2 <- importance2

imp2$var.name <- recode(imp2$var.name,"fairelcc" = "FairElection")
imp2$var.name <- recode(imp2$var.name,"cong" = "Incongruence")
imp2$var.name <- recode(imp2$var.name,"cttresa" = "FairCourt")
imp2$var.name <- recode(imp2$var.name,"fairelc" = "ElectionImp.")
imp2$var.name <- recode(imp2$var.name,"incpart" = "Winner/loser")
imp2$var.name <- recode(imp2$var.name,"gvexpdc" = "Gov.Com.")
# imp2$var.name <- recode(imp2$var.name,"FairElection" = "ElectionImp")
test <- imp2
cntry <- test$colnames[-1,]
test$cntry <- colnames(test)

impplot <- imp2 %>%
  arrange(NO) %>%
  mutate(var.name=factor(var.name, levels=var.name)) %>%
  ggplot(aes (x = NO, y = var.name, colour = "NO")) +
  # geom_point(aes(x = AL, y = var.name, colour = "AL")) +
  geom_point(aes(x = BE, y = var.name, colour = "BE")) +
  geom_point(aes(x = BG, y = var.name, colour = "BG")) +
  geom_point(aes(x = CH, y = var.name, colour = "CH")) +
  geom_point(aes(x = CY, y = var.name, colour = "CY")) +
  geom_point(aes(x = CZ, y = var.name, colour = "CZ")) +
  geom_point(aes(x = DE, y = var.name, colour = "DE")) +
  geom_point(aes(x = DK, y = var.name, colour = "DK")) +
  geom_point(aes(x = EE, y = var.name, colour = "EE")) +
  geom_point(aes(x = ES, y = var.name, colour = "ES")) +
  geom_point(aes(x = FI, y = var.name, colour = "FI")) +
  geom_point(aes(x = FR, y = var.name, colour = "FR")) +
  geom_point(aes(x = GB, y = var.name, colour = "GB")) +
  geom_point(aes(x = HU, y = var.name, colour = "HU")) +
  geom_point(aes(x = IE, y = var.name, colour = "IE")) +
  geom_point(aes(x = IL, y = var.name, colour = "IL")) +
  geom_point(aes(x = IS, y = var.name, colour = "IS")) +
  geom_point(aes(x = IT, y = var.name, colour = "IT")) +
  geom_point(aes(x = LT, y = var.name, colour = "LT")) +
  geom_point(aes(x = NL, y = var.name, colour = "NL")) +
  geom_point(aes(x = NO, y = var.name, colour = "NO")) +
  geom_point(aes(x = PL, y = var.name, colour = "PL")) +
  geom_point(aes(x = PT, y = var.name, colour = "PT")) +
  geom_point(aes(x = SE, y = var.name, colour = "SE")) +
  geom_point(aes(x = SI, y = var.name, colour = "SI")) +
  geom_point(aes(x = SK, y = var.name, colour = "SK")) +
  geom_jitter(position = position_jitter(height = 1)) +
  ylab("") +
  xlab("Importance") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  # geom_jitter(width = 0.25) +
  theme_m() + 
  labs(color = "")
impplot 

ggsave2(impplot, device = "pdf", width = 6, height = 5, filename = "~/OneDrive - University of Bergen/PhD 2017-2021/SWD Paper/Figures/impplot_cntry.pdf")

#### MELT DATA ####
library(data.table)

imp <- transpose(imp2)
imp
colnames(imp) = imp[1, ] # the first row will be the header
imp = imp[-1, ] 
imp[] <- lapply(imp, function(x) as.numeric(as.character(x)))

#### CLACULATE CI ####

get_CI_half_width <- function(x, prob) {
  n <- length(x)
  z_t <- qt(1 - (1 - prob) / 2, df = n - 1)
  z_t * sd(x) / sqrt(n)
}

lower <- function(x, prob = 0.95) {
  mean(x) - get_CI_half_width(x, prob)
}

upper <- function(x, prob = 0.95) {
  mean(x) + get_CI_half_width(x, prob)
}

# impCI <- imp %>% 
#   summarise_all(funs(mean, sd, min, max, lower, upper))

impCI <- lapply(imp, function(x) c( "Stand dev" = sd(x), 
                                    "Mean"= mean(x,na.rm=TRUE),
                                    # "n" = length(x),
                                    # "Median" = median(x),
                                    # "CoeffofVariation" = sd(x)/mean(x,na.rm=TRUE),
                                    # "Minimum" = min(x),
                                    # "Maximun" = max(x),
                                    # "Upper Quantile" = quantile(x,1),
                                    # "LowerQuartile" = quantile(x,0),
                                    "LowerCI" = lower(x),
                                    "UpperCI" = upper(x)
)
)

impCI <- as.data.frame(impCI)
ImpCI <- transpose(impCI)
colnames(ImpCI) <- rownames(impCI)
rownames(ImpCI) <- colnames(impCI)
ImpCI$variable <- rownames(ImpCI)

#### PLOT AGG. IMPORTANCE DATA ####

CIplot <- ImpCI %>%
  mutate(
    variable = case_when(
      variable == "Gov..Com." ~ "Gov.Com.",
      variable == "Winner.loser" ~ "Winner/Loser",
      TRUE ~ variable
    )
  ) %>% as.data.frame() %>% 
  ggplot(aes (x = Mean, y = reorder(variable, Mean))) +
  geom_point() +
  geom_errorbarh(aes(xmax = UpperCI, xmin = LowerCI), height = 0) +
  ylab("") +
  xlab("Importance") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = seq(0,1.4, 0.20),
                     limits = c(-0.1,1.4))  +
  theme_m()
CIplot 
ggsave2(CIplot, device = "pdf", width = 6, height = 4, filename = "~/OneDrive - University of Bergen/PhD 2017-2021/SWD Paper/Figures/Feat_imp_all.pdf")

##########################################################################
############################# Partial dependecies ########################
##########################################################################
# DEFINE TASK 
ESS6_cntry <- ESS6 %>% 
  dplyr::select(!c(cntry, LRSP, gvexpdc)) %>%
  dplyr::rename(
    WinnerLoser = incpart, 
    FairElection = fairelcc,
    FairCourt = cttresa,
    ElectionImp = fairelc,
    Incongruence = cong) #%>% 
# slice_sample(n = 100)
regr.task <-  makeRegrTask(data = ESS6_cntry, target = "stfdem")
lrn <- makeLearner("regr.randomForest",predict.type = "se")
mod <- train(lrn, regr.task)
pd <- generatePartialDependenceData(mod, regr.task,
                                    # , features = c("SatEcon")
)
save(pd, file =  "pd_mod.Rdata")
load("pd_mod.Rdata")
# pd_df <- pd[["data"]]
# pd_var <- pd_df %>% 
#   select(!c(lower:upper))

plotPartialDependence(pd) + 
  # geom_ribbon(aes(ymax = upper, ymin = lower), alpha = .25) +
  scale_y_continuous(
    limits = c(4,7),
    breaks = seq(0,10, by = 1)
  ) +
  # scale_x_continuous(
  # limits = c(0,10),
  # # breaks = seq(0,10, by = 1)
  # )+
  theme_m()
##### ALTERNATIVES PDP----------------------------------------------------
imp2$var.name <- recode(imp2$var.name,"fairelcc" = "FairElection")
imp2$var.name <- recode(imp2$var.name,"cttresa" = "FairCourt")
imp2$var.name <- recode(imp2$var.name,"fairelc" = "ElectionImp.")
imp2$var.name <- recode(imp2$var.name,"incpart" = "Winner/loser")
imp2$var.name <- recode(imp2$var.name,"GovCommunication" = "Gov. Com.")
# install.packages("randomForest")
# install.packages("pdp")
library(randomForest)  # for fitting random forests
library(pdp)           # for partial dependence plots
library(vip)
ess <- ESS6 %>% 
  dplyr::select(!c(cntry, LRSP)) %>% 
  mutate(
    Female = as.factor(Female),
    Unemp = as.factor(Unemp),
    incpart = as.factor(incpart)
  )
swd_rf <- randomForest(stfdem ~ ., d = ess, importance = TRUE)
vip(swd_rf)
# , bar = FALSE, horizontal = FALSE, size = 1.5)

# Gen. PDP
econ <- partial(swd_rf, pred.var = c("SatEcon"), plot = F)
incong <- partial(swd_rf, pred.var = c("cong"), plot = F)
wl <- partial(swd_rf, pred.var = c("incpart"), plot = F)
pov <- partial(swd_rf, pred.var = c("Poverty"), plot = F)
inq <- partial(swd_rf, pred.var = c("Inequality"), plot = F)
elimp <- partial(swd_rf, pred.var = c("fairelc"), plot = F)
crsm <- partial(swd_rf, pred.var = c("Crtsame"), plot = F)

#SW Economy
p_econ <- econ %>% 
  ggplot(aes(x = SatEcon, y = yhat)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(
    breaks = seq(0,10, by = 1)
  ) +
  scale_y_continuous(
    breaks = seq(0,10, by = .5),
    limits = c(4,7)
  ) +
  labs(x = "Satisfaction with the economy",y = "") +
  theme_m()
# p_econ
# Incongruence
df <- incong
yhat <- df$yhat
cong <- df$cong
df <- cbind(cong, yhat)  
df <- as.data.frame(df) %>% 
  mutate(
    var = "Incongruence",
    pred = cong
  ) %>% 
  dplyr::select(!c(cong))
df <- df %>%
  mutate(
    pred = round(pred)
  ) %>% 
  group_by(pred) %>% 
  slice(1) %>% 
  as.data.frame()

win <- wl
yhat <- win$yhat
pred <- win$incpart
var <- "Winner/Loser"
win <- cbind.data.frame(pred,yhat, var)
df <- rbind(df,win)  
df <- df %>% 
  mutate(
    pred = as.numeric(pred),
    pred = case_when(
      var == "Winner/Loser" & pred == 0 ~ 8,
      var == "Winner/Loser" & pred == 1 ~ 0,
      TRUE ~ pred
    )
  )


p_cong <- df %>% 
  ggplot(aes(x = pred, y = yhat, shape = var)) +
  geom_line() +
  geom_point(show.legend = FALSE)+
  scale_shape_manual(
    values = c(16,15)
  )+
  scale_x_continuous(
    breaks = seq(0,10, by = 1),
    limits = c(0,8.5)
  ) +
  scale_y_continuous(
    breaks = seq(0,10, by = .25),
    limits = c(5,6)
  ) +
  labs(x = "Incongruence", y = "", shape = "") + 
  theme_m()
p_cong
# Winner
lab <- c("Loser", "Winner")
p_wl <- wl %>% 
  ggplot(aes(x = incpart, y = yhat)) +
  geom_point() +
  scale_y_continuous(
    breaks = seq(0,10, by = .25),
    limits = c(5,6) )+
  scale_x_discrete(
    labels = lab
  ) +
  labs(x = "Outcome of election", y = "") +
  theme_m()
# p_wl

#Poverty
p_pov <- pov %>% 
  ggplot(aes(x = Poverty, y = yhat)) +
  geom_line() + 
  geom_point()+
  scale_x_continuous(
    breaks = seq(0,10, by = 1)
  ) +
  scale_y_continuous(
    breaks = seq(0,10, by = .25),
    limits = c(5,6)
  ) +
  labs(x = "Responsiveness to poverty", y = "") +
  theme_m()
# p_pov
#Poverty
p_inq <- inq %>% 
  ggplot(aes(x = Inequality, y = yhat)) +
  geom_line() + 
  geom_point()+
  scale_x_continuous(
    breaks = seq(0,10, by = 1)
  ) +
  scale_y_continuous(
    breaks = seq(0,10, by = .25),
    limits = c(5,6)
  ) +
  labs(x = "Responsiveness to inequality", y = "") +
  theme_m()
# p_inq
#ElectionImp
p_elimp <- elimp %>% 
  ggplot(aes(x = fairelc, y = yhat)) +
  geom_line() + 
  geom_point()+
  scale_x_continuous(
    breaks = seq(0,10, by = 1)
  ) +
  scale_y_continuous(
    breaks = seq(0,10, by = .25),
    limits = c(5,6)
  ) +
  labs(x = "National elections are free and fair", y = "") +
  theme_m()
# p_elimp
# Court same
p_crsm <- crsm %>% 
  ggplot(aes(x = Crtsame, y = yhat)) +
  geom_line() + 
  geom_point()+
  scale_x_continuous(
    breaks = seq(0,10, by = 1)
  ) +
  scale_y_continuous(
    breaks = seq(0,10, by = .25),
    limits = c(5,6)
  ) +
  labs(x = "Equal treatment in courts", y = "") +
  theme_m()
# p_crsm
# library(cowplot)
p <- plot_grid(p_econ, p_elimp, p_crsm, p_inq, p_pov, p_cong, labels = "auto", label_size = 12)
p
pdf(height = 4, width = 8, file = "/Users/TroyBroderstad/OneDrive - University of Bergen/PhD 2017-2021/SWD Paper/Figures/pdp_plot.pdf")
plot(p)
dev.off()
cowplot::ggsave2(plot = p, encoding="MacRoman", device = "pdf", height = 4, width = 8, "/Users/TroyBroderstad/OneDrive - University of Bergen/PhD 2017-2021/SWD Paper/Figures/pdp_plot.pdf", dpi = 299)
##########################################################################
############################### RMSE FOR COUNTRY ########################
##########################################################################

#### TUNING ####
# RUN ONCE

set.seed(2021)
cntry <- as.character(unique(ESS6$cntry))
regr.task <- list()
bmr <- list()
perf <- list()

# Define a search space for each learner's parameter
ps_ksvm = makeParamSet(
  makeNumericParam("sigma", lower = -12, upper = 12, trafo = function(x) 2^x)
)
ps_rf = makeParamSet(
  makeIntegerParam("num.trees", lower = 1L, upper = 200L)
)
# Choose a resampling strategy
rdesc = makeResampleDesc("CV", iters = 10)
# Choose a performance measure
meas = rmse
# Choose a tuning method
ctrl = makeTuneControlCMAES(budget = 100L)
# Make tuning wrappers
tuned.ksvm = makeTuneWrapper(learner = "regr.ksvm", resampling = rdesc, measures = meas,
                             par.set = ps_ksvm, control = ctrl, show.info = FALSE)
tuned.rf = makeTuneWrapper(learner = "regr.ranger", resampling = rdesc, measures = meas,
                           par.set = ps_rf, control = ctrl, show.info = FALSE)
# Four learners to be compared
lrns = list(makeLearner("regr.lm"), tuned.ksvm,
            tuned.rf)


## START LOOP
for (i in 1: length(cntry)){
  data <- ESS6
  data <- data[data$cntry == cntry[i], ]
  data$cntry <- NULL
  
  # Define task
  regr.task[[i]] <-  makeRegrTask(data = data, target = "stfdem")
  
  # Conduct the benchmark experiment
  bmr[[i]] <-  benchmark(learners = lrns, tasks = regr.task[[i]], 
                         resamplings = rdesc, measures = rmse, show.info = FALSE,
                         keep.pred = FALSE, models = FALSE)
}


#### EXTRACT AGG. RMSE FOR COUNTRY LEVEL ####
regr.lm <- vector()
regr.ranger <- vector()
regr.ksvm <- vector()
for (i in 1: length(bmr)){
  
  regr.lm <- c(regr.lm, bmr[[i]][["results"]][["data"]][["regr.lm"]][["aggr"]][["rmse.test.rmse"]])
  regr.ranger <- c(regr.ranger, bmr[[i]][["results"]][["data"]][["regr.ksvm.tuned"]][["aggr"]][["rmse.test.rmse"]])
  regr.ksvm <- c(regr.ksvm, bmr[[i]][["results"]][["data"]][["regr.ranger.tuned"]][["aggr"]][["rmse.test.rmse"]])
}

# Build DF for RMSE
rmse_df <- as.data.frame(cbind(regr.lm, regr.ranger, regr.ksvm))


rmse_ci <- as.data.frame(rmse_ci)
rmse_ci <- t(rmse_ci)

rmse_ci$variable <-  rownames(rmse_ci)

rmse_ci <- as.data.frame(rmse_ci)



colnames(rmse_ci) <- rownames(rmse_ci)
rownames(rmse_ci) <- colnames(rmse_ci)
rmse_ci$variable <- rownames(rmse_ci)

rmse_ci$variable <- NULL

#### PLOT RMSE FOR COUNTRY ####


rmse_plot <- rmse_ci %>%
  ggplot(aes (x = Mean, y = reorder(variable, Mean))) +
  geom_point() +
  geom_errorbarh(aes(xmax = UpperCI, xmin = LowerCI), height = 0.1) +
  ylab("") +
  xlab("RMSE") +
  geom_vline(xintercept = 1.722, linetype = "dashed") +
  scale_x_continuous(breaks = seq(0,2, 0.05),
                     limits = c(1.5,2))  +
  theme_m()
rmse_plot 




#### EXTRACT RMSE FOR IND. CNRTY ####

lm <- vector()
ranger <- vector()
ksvm <- vector()
for (i in 1: length(bmr)){
  
  lm <- c(lm, bmr[[i]][["results"]][["data"]][["regr.lm"]][["measures.test"]][["rmse"]])
  ranger <- c(ranger, bmr[[i]][["results"]][["data"]][["regr.ranger.tuned"]][["measures.test"]][["rmse"]])
  ksvm <- c(ksvm, bmr[[i]][["results"]][["data"]][["regr.ksvm.tuned"]][["measures.test"]][["rmse"]])
}
# Build DF for RMSE
rmsedf <- as.data.frame(cbind(lm, ranger, ksvm))

rmsedf$id <- 1:nrow(rmsedf)

rmsedf <- rmsedf %>% 
  mutate(
    cntry = case_when(
      id == 1:10 ~ "AL",
      id == 11:20 ~ "BE",
      id == 21:30 ~ "BG",
      id == 31:40 ~ "CY",
      id == 41:50 ~ "CZ",
      id == 51:60 ~ "DK",
      id == 61:70 ~ "EE",
      id == 71:80 ~ "FI",
      id == 81:90 ~ "FR",
      id == 91:100 ~ "DE",
      id == 101:110 ~ "HU",
      id == 111:120 ~ "IS",
      id == 121:130 ~ "IE",
      id == 131:140 ~ "IL",
      id == 141:150 ~ "IT",
      id == 151:160 ~ "XK",
      id == 161:170 ~ "LT",
      id == 171:180 ~ "NL",
      id == 181:190 ~ "NO",
      id == 191:200 ~ "PL",
      id == 201:210 ~ "PT",
      id == 211:220 ~ "RU",
      id == 221:230 ~ "SK",
      id == 231:240 ~ "SI",
      id == 241:250 ~ "ES",
      id == 251:260 ~ "SE",
      id == 261:270 ~ "CH",
      id == 271:280 ~ "UA",
      id == 281:290 ~ "GB"
    )
  )

# COMPUTE CI 
keep <- c("AL","XK", "RU", "UA")

rmsedf_cntry <- rmsedf %>% 
  group_by(cntry) %>% 
  summarise_at(vars(lm, ranger, ksvm), funs(mean, lower, upper)) 
rmsedf_cntry <- rmsedf_cntry %>% 
  as.data.frame() %>% 
  # dplyr::filter(cntry == "AL"| cntry != "XK"| cntry != "RU"| cntry != "UA") 
  dplyr::filter(!cntry %in% keep)

#### PLOT RMSE BY COUNTRY ####

# RANGER
ranger_plot_cntry <- rmsedf_cntry %>%
  # dplyr::filter(!cntry = "AL"| !cntry = "XK"| !cntry = "RU"| !cntry = "UA") %>% 
  ggplot(aes (x = ranger_mean, y = reorder(cntry, ranger_mean))) +
  geom_point() +
  geom_errorbarh(aes(xmax = ranger_upper, xmin = ranger_lower), height = 0) +
  ylab("") +
  xlab("") +
  geom_vline(xintercept = 1.723, linetype = "dashed") +
  scale_x_continuous(breaks = seq(0,3, 0.5),
                     limits = c(1,2.5))  +
  ggtitle("RF") +
  theme_m()
ranger_plot_cntry

# LM PLOT
lm_plot_cntry <- rmsedf_cntry %>%
  ggplot(aes (x = lm_mean, y = reorder(cntry, lm_mean))) +
  geom_point() +
  geom_errorbarh(aes(xmax = lm_upper, xmin = lm_lower), height = 0) +
  ylab("") +
  xlab("") +
  geom_vline(xintercept = 1.617, linetype = "dashed") +
  scale_x_continuous(breaks = seq(0,3, 0.5),
                     limits = c(1,2.5))  +
  ggtitle("LM") +
  theme_m(axis.line.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
# lm_plot_cntry

# KSVM PLOT
ksvm_plot_cntry <- rmsedf_cntry %>%
  ggplot(aes (x = ksvm_mean, y = reorder(cntry, ksvm_mean))) +
  geom_point() +
  geom_errorbarh(aes(xmax = ksvm_upper, xmin = ksvm_lower), height = 0) +
  ylab("") +
  xlab("RMSE") +
  geom_vline(xintercept = 1.714, linetype = "dashed") +
  scale_x_continuous(breaks = seq(0,3, 0.5),
                     limits = c(1,2.5))  +
  ggtitle("KSVM") +
  theme_m(axis.line.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
# ksvm_plot_cntry

library(cowplot)

plot_grid(ranger_plot_cntry, ksvm_plot_cntry, lm_plot_cntry, ncol = 3)


#### FULL MODEL ####

# Define a search space for each learner's parameter
ps_ksvm = makeParamSet(
  makeNumericParam("sigma", lower = -12, upper = 12, trafo = function(x) 2^x)
)
ps_rf = makeParamSet(
  makeIntegerParam("num.trees", lower = 1L, upper = 200L)
)
# Choose a resampling strategy
rdesc = makeResampleDesc("CV", iters = 10)
# Choose a performance measure
meas = rmse
# Choose a tuning method
ctrl = makeTuneControlCMAES(budget = 100L)
# Make tuning wrappers
tuned.ksvm = makeTuneWrapper(learner = "regr.ksvm", resampling = rdesc, measures = meas,
                             par.set = ps_ksvm, control = ctrl, show.info = FALSE)
tuned.rf = makeTuneWrapper(learner = "regr.ranger", resampling = rdesc, measures = meas,
                           par.set = ps_rf, control = ctrl, show.info = FALSE)
# Four learners to be compared
lrns = list(makeLearner("regr.lm"), tuned.ksvm,
            tuned.rf)



# Define task
ESS6 <- ESS6 %>% 
  dplyr::filter(!cntry=="AL"|!cntry=="XK"|!cntry=="RU"|!cntry=="UA") %>% 
  dplyr::select(!cntry)
regr.task_c <-  makeRegrTask(data = ESS6, target = "stfdem")

# Conduct the benchmark experiment
bmr_c <-  benchmark(learners = lrns, tasks = regr.task_c, 
                    resamplings = rdesc, measures = rmse, show.info = FALSE,
                    keep.pred = FALSE, models = FALSE)

# FEATURE IMPORTANCE 
lrn <- makeLearner("regr.ranger", importance = c("permutation"))
mod_c <- train(lrn, regr.task_c)
importance <- getFeatureImportance(mod_c, nmc = 50L, interaction = TRUE, local = TRUE)
# importance <- gather(importance[[i]]$res, "var.name", "importance")

imp_c <- gather(importance$res,"c", "var.name", "importance")
imp_c <- imp_c %>% 
  mutate(
    importance = var.name,
    var.name = variable
  )
imp_c
imp_c$var.name <- recode(imp_c$var.name,"fairelcc" = "Fair Election")
imp_c$var.name <- recode(imp_c$var.name,"cong" = "Incongruence")
imp_c$var.name <- recode(imp_c$var.name,"cttresa" = "Fair Court")
imp_c$var.name <- recode(imp_c$var.name,"fairelc" = "Election Imp.")
imp_c$var.name <- recode(imp_c$var.name,"incpart" = "Winner/loser")
imp_c$var.name <- recode(imp_c$var.name,"GovCommunication" = "Gov.Com")
imp_c

# imp2$var.name <- recode(imp2$var.name,"fairelcc" = "FairElection")
# imp2$var.name <- recode(imp2$var.name,"cong" = "Incongruence")
# imp2$var.name <- recode(imp2$var.name,"cttresa" = "FairCourt")
# imp2$var.name <- recode(imp2$var.name,"fairelc" = "ElectionImp.")
# imp2$var.name <- recode(imp2$var.name,"incpart" = "Winner/loser")
# imp2$var.name <- recode(imp2$var.name,"gvexpdc" = "Gov.Com.")

imp_full <- imp_c %>% 
  dplyr::filter(var.name != "gvexpdc") %>% 
  ggplot(aes(x = importance, y = reorder(var.name, importance))) +
  geom_point() +
  geom_vline(aes(xintercept = 0), linetype = "dotted")+
  scale_x_continuous(
    breaks = seq(0,1.5,.25)
  ) +
  xlab("Importance") +
  ylab("") +
  theme_m()
ggsave(imp_full, device = "pdf", width = 6, height = 5, 
       filename = "impplot_full.pdf")


######## DESC. STATS ###########
### SUMMARY FUNCTION CI #######

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

ESS_agg <- summarySE(ESS6, measurevar = "stfdem", groupvars = "cntry")

#### Desc stat. table ####
mu <- mean(ESS_agg$stfdem)
swd_cntry <- ESS_agg %>%
  ggplot(aes (x = stfdem, y = reorder(cntry, stfdem))) +
  geom_point() +
  geom_errorbarh(aes(xmax = stfdem + (1.96 * se),
                     xmin = stfdem - (1.96 * se)), height = 0.1) +
  ylab("") +
  xlab("") +
  # ggtitle("Satisfaction with democracy") +
  scale_x_continuous(
    limits = c(3, 8),
    breaks = round(seq(3, 8, .5), 2)) +
  geom_vline(xintercept = mu, linetype = "dashed") +
  theme_m()

swd_cntry

# ESS6 %>% group_by(cntry) %>% 
stargazer(ESS6, summary.stat = c("mean","sd", "min", "max"),
          covariate.labels = 
            c("Satisfaction w/democracy (SatDem)",
              "Satisfaction w/the Economy (SatEcon)",
              "Electoral winner/loser",
              "Female",
              "Age",
              "Unemployment (Unemp)",
              "Religiosity",
              "Social trust (SocTrust)",
              "Income decile (IncDec)",
              "Years of education (YearsEdu) ",
              "Equal treatment in courts (CrtSame)",
              "Elections are fair (FairElection)",
              "Courts are fair (FairCourt)",
              "Fair elections are important (ElectionImp)",
              "Government communication (GovCom)",
              "Poverty",
              "Inequality",
              "Incongruence"
            ))
