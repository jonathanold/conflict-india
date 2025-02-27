## Spatial Analysis for Extended Essay
setwd("~/Library/CloudStorage/GoogleDrive-jonathan_old@berkeley.edu/My Drive/_Berkeley Research/Reservations and Conflict/")
getwd()
#==============
# LOAD PACKAGES
#==============

{
  
  library(extrafont)
  loadfonts()
  ## devtools::install_github('https://github.com/AakaashRao/starbility')
  library(stringi)
  library(grid)
  library(tidyverse)
  library(sf)
  library(rvest)
  library(viridis)
  library(maps)
  library(ggplot2)
  library(countrycode)
  library(dplyr)
  library(foreign)
  library(viridis)
  library(broom)
  library(tikzDevice)
  library(mapproj)
  library("pryr")
  library("rgdal")
  library("rmapshaper")
  install.packages("pscl")
  require(pscl)
  require(MASS)
  require(boot)
  require(lfe)
  x <- c("ggplot2", "stargazer", "foreign", "xtable", "splitstackshape", "dplyr", "tidyr", "zoo",
         "maptools", "plm", "lmtest", "MASS", "spdep", "rgdal", "rgeos", "classInt", "gstat", "readstata13",
         "viridis", "margins", "lmtest", "car", "splm")
  lapply(x, require, character.only = TRUE)
  rm(x)
  library(starbility)
  
  rm(list=ls(all=T))
}

#============
# Load data
#============


## Read in Conflict data
conflict <- read.csv("./Data/ACLED/acled_india_nostate.csv", sep=";") ## india_new.csv ## acled_india_3: Caste stuff
# Cleaning conflict data
{
  conflict$one <- 1
  cfc <- cbind(conflict$longitude, conflict$latitude)
  cf <- SpatialPointsDataFrame(coords = cfc, data = conflict,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  
  
  ## Read in Map data
  polyg = readOGR(dsn="Maps/India_Maps_New/assembly-constituencies/",layer="India_AC")
  names(polyg)
  
  
  # Get events: Spatial merge
  test <- over(polyg, cf[,"one"], fn=sum)
  polyg$one <- test$one
  polyg$one[is.na(polyg$one)] <- 0
  polyg$one <- as.numeric(polyg$one)
  
  # Get fatalities
  test <- over(polyg, cf[,"fatalities"], fn=sum)
  polyg$fat <- test$fatalities
  polyg$fat[is.na(polyg$fat)] <- 0
  polyg$fat <- as.numeric(polyg$fat)
  
  # Simplify polygons to speed up analysis
  simplepoly <- ms_simplify(polyg)
  polyg <- simplepoly
  
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # 0.000   0.000   0.000   1.702   1.000 337.000 
  # > summary(polyg@data$one)
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # 0.00    1.00    3.00   17.51   10.00 2175.00 
}



## Main analysis starts from here
## First, merge data with other data on assembly constituencies, using the dataset exported from Stata
{
  assembly_data <- read.dta("./Data/_gen/constituency/const_data_r.dta")
  complete_data <- merge(x=polyg, y=assembly_data, by.x="Map_ID", by.y="map_id")
  
  
  complete_data@data$prop_sc <- complete_data@data$pc01_pca_p_sc/complete_data@data$pc01_pca_tot_p*100
  complete_data@data$prop_schools <- complete_data@data$pc01_vd_p_sch/complete_data@data$pc01_pca_tot_p*100
  complete_data@data$one <- complete_data@data$one.x
  
  complete_data@data$acled_totalevents <- complete_data@data$one
  complete_data@data$acled_edummy <- 0
  complete_data@data$acled_edummy[complete_data@data$acled_totalevents!=0] <- 1
  complete_data@data$acled_fdummy <- 0
  complete_data@data$acled_fdummy[complete_data@data$fat!=0] <- 1
  
  
  complete_data@data$sample_1[is.na(complete_data@data$sample_1)] <- 0
  complete_data@data$sample_2[is.na(complete_data@data$sample_2)] <- 0
  complete_data@data$sample_3[is.na(complete_data@data$sample_3)] <- 0
  complete_data@data$sample_4[is.na(complete_data@data$sample_4)] <- 0
  complete_data@data$sample_5[is.na(complete_data@data$sample_5)] <- 0
  
  
  data_subsetted <- complete_data[complete_data@data$sample_3==1,]
  # 3 ok
  
  
  ## Only non-missing data - required manually for some of the spatial methods
  data_subsetted <- data_subsetted[is.na(data_subsetted@data$acled_totalevents)==F & 
                                     is.na(data_subsetted@data$sc_prop_01_norm)==F & 
                                     is.na(data_subsetted@data$perc_sc)==F & 
                                     is.na(data_subsetted@data$pc01_pca_tot_p_norm)==F &
                                     is.na(data_subsetted@data$pc01_vd_p_sch_norm)==F &
                                     is.na(data_subsetted@data$pc01_vd_tar_road_norm)==F,]
  
  data_subsetted@data$sc_prop_01_norm <- (data_subsetted@data$prop_sc - mean(data_subsetted@data$prop_sc))/sqrt(var(data_subsetted@data$prop_sc))
  m1 <- mean(data_subsetted@data$prop_sc)
  v1 <- sqrt(var(data_subsetted@data$prop_sc))
}

## Stability plots

# Regressions for main text and poisson regressions for appendix
{
  
  emodel <-  acled_totalevents ~ sc_reserved + sc_prop_01_norm + I(sc_reserved*sc_prop_01_norm) +  
    pc01_pca_tot_p_norm + 
    pc01_vd_p_sch_norm + 
    pc01_vd_tar_road_norm 
  fmodel <-  fat ~ sc_reserved + sc_prop_01_norm +  I(sc_reserved*sc_prop_01_norm) +  
    pc01_pca_tot_p_norm +
    pc01_vd_p_sch_norm + 
    pc01_vd_tar_road_norm 
  logmodel <- log(acled_totalevents+1) ~ sc_reserved + sc_prop_01_norm +  I(sc_reserved*sc_prop_01_norm) +
    pc01_pca_tot_p_norm + 
    pc01_vd_p_sch_norm + 
    pc01_vd_tar_road_norm 
  logmodelf <- log(fat+1) ~ sc_reserved + sc_prop_01_norm +  I(sc_reserved*sc_prop_01_norm) +
    pc01_pca_tot_p_norm +
    pc01_vd_p_sch_norm + 
    pc01_vd_tar_road_norm 
  emodel1 <-  acled_totalevents ~ sc_reserved + sc_prop_01_norm +  
    pc01_pca_tot_p_norm +
    pc01_vd_p_sch_norm + 
    pc01_vd_tar_road_norm 
  fmodel1 <-  fat ~ sc_reserved + sc_prop_01_norm +  
    pc01_pca_tot_p_norm +
    pc01_vd_p_sch_norm + 
    pc01_vd_tar_road_norm 
  logmodel1 <- log(acled_totalevents+1) ~ sc_reserved + sc_prop_01_norm +  
    pc01_pca_tot_p_norm + 
    pc01_vd_p_sch_norm + 
    pc01_vd_tar_road_norm 
  logmodelf1 <- log(fat+1) ~ sc_reserved + sc_prop_01_norm +  
    pc01_pca_tot_p_norm +
    pc01_vd_p_sch_norm + 
    pc01_vd_tar_road_norm 
  
  
  summary(nb1 <- glm.nb(formula="emodel" ,
                        data=data_subsetted))
  summary(nb2 <- glm.nb(formula="fmodel" ,
                        data=data_subsetted))
  summary(ps1 <- glm(formula="emodel" ,
                     data=data_subsetted, family="poisson"))
  summary(ps2 <- glm(formula="fmodel" ,
                     data=data_subsetted, family="poisson"))
  summary(ols1 <- lm(formula = "logmodel" ,
                     data=data_subsetted))
  summary(ols2 <- lm(formula = "logmodelf" ,
                     data=data_subsetted))
  
  summary(nb11 <- glm.nb(formula="emodel1" ,
                         data=data_subsetted))
  summary(nb21 <- glm.nb(formula="fmodel1" ,
                         data=data_subsetted))
  summary(ps11 <- glm(formula="emodel1" ,
                      data=data_subsetted, family="poisson"))
  summary(ps21 <- glm(formula="fmodel1" ,
                      data=data_subsetted, family="poisson"))
  summary(ols11 <- lm(formula = "logmodel1" ,
                      data=data_subsetted))
  summary(ols21 <- lm(formula = "logmodelf1" ,
                      data=data_subsetted))
  
  mean(data_subsetted@data$acled_totalevents)
  var(data_subsetted@data$acled_totalevents)
  
  mean(data_subsetted@data$fat)
  var(data_subsetted@data$fat)
  
  
  stargazer(ols11, ols1, ols21, ols2,  nb11, nb1, nb21, nb2,
            title = "Robustness Check: Constituency assemblies, alternative sample",
            covariate.labels = c("\\textbf{Seat reserved for SC}",
                                 "Percent SC population",
                                 "\\textbf{Reserved $\\times$ SC pop}",
                                 "\\addlinespace Population in 1000",
                                 "Primary Schools per 1000",
                                 "Percent paved roads",
                                 "Constant"),
            no.space=TRUE,
            column.labels   = c("OLS: log(Events+1)", "OLS: log(Fatalities+1)", "NB: Events", "NB: Fatalities"),
            column.separate = c(2, 2, 2, 2),
            model.names = F,
            dep.var.caption = "",
            dep.var.labels.include = F,
            omit.stat=c("ser", "theta", "f" ),
            notes = c("Results from regressions of conflict events/fatalities for constituency assemblies.",
                      "OLS is for ordinary least squares, NB for negative binomial regression models.",
                      "Alternative sample: Constituencies within 5 percentage points SC population margin around cutoff",
                      "Signficance levels: $^*10\\%, ^{**}5\\%, ^{***}1\\%$.\\label{tab:assemblysample}"),
            notes.align = "l",
            notes.append=F,
            notes.label = "\\textbf{Note:}",
            out="./Output/tables/assemblyreg_sample.tex")
  
  
}

# Make predicted value graphs for main text
{
  # DEFINE MODELS AGAIN
  emodel <-  acled_totalevents ~ sc_reserved + sc_prop_01_norm + I(sc_reserved*sc_prop_01_norm) +  
    pc01_pca_tot_p_norm +
    pc01_vd_p_sch_norm + 
    pc01_vd_tar_road_norm 
  fmodel <-  fat ~ sc_reserved + sc_prop_01_norm +  I(sc_reserved*sc_prop_01_norm) +  
    pc01_pca_tot_p_norm +
    pc01_vd_p_sch_norm + 
    pc01_vd_tar_road_norm 
  
  logmodel <- log(acled_totalevents+1) ~ sc_reserved + sc_prop_01_norm +  I(sc_reserved*sc_prop_01_norm) +
    pc01_pca_tot_p_norm +
    pc01_vd_p_sch_norm + 
    pc01_vd_tar_road_norm 
  
  logmodelf <- log(fat+1) ~ sc_reserved + sc_prop_01_norm +  I(sc_reserved*sc_prop_01_norm) +
    pc01_pca_tot_p_norm +
    pc01_vd_p_sch_norm + 
    pc01_vd_tar_road_norm 
  ## Define function to make the plot
  make_prediction <- function(df, model, type) {
    x <- df
    y <- model
    z <- type
    
    if (z=="nb") {
      summary(nb <- glm.nb(formula=y ,
                           data=x))
    }  else if (z=="ps") {
      summary(nb <- glm(formula=y ,
                        data=x,
                        family="poisson"))
    }  else if (z=="logit") {
      summary(nb <- glm(formula=y ,
                        data=x,
                        family=binomial(link="logit")))
    } else if (z=="ols") {
      summary( nb <- lm(formula = y, data=x))  
      
    }
    
    
    
    if (y=="edmodel") {
      tt <- c("Predicted Pr(conflict event)")
      lim <- c(0,0.7)
    }  else if (y=="fdmodel") {
      tt <- c("Predicted Pr(conflict fatality)")
      lim <- c(0,0.7)
    }  else if (y=="emodel") {
      tt <- c("Predicted no. of conflict events")
      lim <- c(0,7)
    }  else if (y=="fmodel") {
      tt <- c("Predicted no. of fatalities")
      lim <- c(0,0.7)
    } else if (y=="logmodel") {
      tt <- c("Predicted no. of events")
      lim <- c(0,7)
    } else if (y=="logmodelf") {
      tt <- c("Predicted no. of fatalities")
      lim <- c(0,0.7)
    } 
    
    pr <- data.frame(
      sc_prop_01_norm = rep(seq(from = quantile(x$sc_prop_01_norm, na.rm=T, probs=0.1),
                                to = quantile(x$sc_prop_01_norm, na.rm=T, probs=0.9),
                                length.out = 100),
                            2),
      sc_reserved = factor(rep(0:1, each = 100), levels = 0:1, labels =
                             levels(as.factor(x$sc_reserved)))
    )
    pr$pc01_pca_tot_p_norm = mean(x$pc01_pca_tot_p_norm, na.rm=T)
    pr$pc01_vd_p_sch_norm = mean(x$pc01_vd_p_sch_norm, na.rm=T)
    pr$pc01_vd_tar_road_norm = mean(x$pc01_vd_tar_road_norm, na.rm=T)
    pr$sc_reserved <- as.numeric(pr$sc_reserved)-1
    pr$sc_reserved_mean <- mean(as.numeric(x$sc_reserved-1), na.rm=T)
    
    if (z!="ols") {
      pr <- cbind(pr, predict.glm(object = nb, newdata = pr, type = "response", se.fit=TRUE))
      pr <- within(pr, {
        p_events <- fit
        LL <- fit - 1.96*se.fit
        UL <- fit + 1.96*se.fit
      })
    } else {
      pr <- cbind(pr, predict.lm(object = nb, newdata = pr, type = "response", se.fit=TRUE))
      pr <- within(pr, {
        p_events <- exp(fit) - 1
        LL <- exp(fit - 1.96*se.fit)-1
        UL <- exp(fit + 1.96*se.fit)-1
      })
    }
    
    pr$sc_reserved <- as.factor(pr$sc_reserved)
    
    graph <-  ggplot(pr, aes(sc_prop_01_norm*v1+m1, p_events, group=sc_reserved))  +
      geom_ribbon(aes(ymin = LL, ymax = UL, fill=sc_reserved), alpha = .4)  +
      geom_line(aes(color=sc_reserved), size = 1) +
      scale_color_manual(values=c("#0a6264", "#db4324"), labels=c("Unreserved", "Reserved")) +
      scale_fill_manual(values=c("#0a6264", "#db4324"), labels=c("Unreserved", "Reserved")) +
      labs(x = "Proportion SC in population", y = tt) +
      theme_minimal() +
      coord_cartesian(xlim=c(12,32), ylim = lim) +
      theme(legend.text = element_text(margin = margin(r = 30, unit = "pt"))) +
      theme(panel.grid.minor = element_blank()) +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(legend.position="bottom", legend.title=element_blank(),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            axis.title.y = element_text(size = 12),
            axis.title.x = element_text(size = 12),
            legend.text = element_text(size = 12))
    return(graph)
    
  }
  
  g1 <- make_prediction(df=data_subsetted, model="emodel", type="nb")
  print(g1)
  tikz(file="./Output/figures/nb_events_sample.tex", standAlone=F,
       width = 5, height = 4)
  print(g1)
  dev.off()
  
  
  g2 <- make_prediction(df=data_subsetted, model="fmodel", type="nb")
  tikz(file="./Output/figures/nb_fatalities_sample.tex", standAlone=F,
       width = 5, height = 4)
  print(g2)
  dev.off()
  
  
  
  
  g5 <- make_prediction(df=data_subsetted, model="emodel", type="ps")
  tikz(file="./Output/figures/ps_events_sample.tex", standAlone=F,
       width = 5, height = 4)
  print(g5)
  dev.off()
  
  
  g6 <- make_prediction(df=data_subsetted, model="fmodel", type="ps")
  tikz(file="./Output/figures/ps_fatalities_sample.tex", standAlone=F,
       width = 5, height = 4)
  print(g6)
  dev.off()
  
  
  
  g7 <- make_prediction(df=data_subsetted, model="logmodel", type="ols")
  tikz(file="./Output/figures/log_events_sample.tex", standAlone=F,
       width = 5, height = 4)
  print(g7)
  dev.off()
  
  
  g8 <- make_prediction(df=data_subsetted, model="logmodelf", type="ols")
  tikz(file="./Output/figures/log_fatalities_sample.tex", standAlone=F,
       width = 5, height = 4)
  print(g8)
  dev.off()
}


