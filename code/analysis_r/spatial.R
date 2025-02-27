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

# Make maps
{
mapdata <- tidy(polyg, region = "Map_ID")
mapdata <- merge(x=mapdata, y=polyg@data, by.x="id", by.y="Map_ID")
mapdata$le <- log(mapdata$one + 1)

mapdata$events <- mapdata$one + 0.05


events <- ggplot(mapdata, aes( x = long, y = lat, group = group )) +
  theme_void() +
  geom_polygon(aes(fill=events)) +
  scale_fill_viridis(trans = "log", breaks=c(0,0.1,1,2,5,10,50,300), labels=c("0", "0", "1", "2", "5", "10", "50", "300+"), name="", guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1) ) +
  theme(
    text = element_text(color = "#22211d", family="Palatino Linotype"),
    plot.background = element_rect(fill = "#ffffff", color = NA),
    panel.background = element_rect(fill = "#ffffff", color = NA),
    legend.background = element_rect(fill = "#ffffff", color = NA),
    legend.title = element_text(size = 12),
    legend.text = element_text(size=12),
    plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 17, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text( size=15, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),
    legend.position = c(0.7, 0.09)
  ) +
  coord_map()
# 
# tikz(file="./Output/figures/map_events.tex", standAlone=F,
#      width = 10, height = 8)
# print(events)
# dev.off()


pdf(file="./Output/figures/map_events.pdf", family="Palatino Linotype",
     width = 8, height = 8)
print(events)
dev.off()


mapdata$fatalities <- mapdata$fat + 0.05

fatalities <- ggplot(mapdata, aes( x = long, y = lat, group = group )) +
  theme_void() +
  geom_polygon(aes(fill=fatalities)) +
  scale_fill_viridis(trans = "log", breaks=c(0,0.1,1,2,5,10,20,100), labels=c("0", "0", "1", "2", "5", "10", "20", "100+"), name="", guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1) ) +
  theme(
    text = element_text(color = "#22211d", family="Palatino Linotype"),
    plot.background = element_rect(fill = "#ffffff", color = NA),
    panel.background = element_rect(fill = "#ffffff", color = NA),
    legend.background = element_rect(fill = "#ffffff", color = NA),
    legend.title = element_text(size = 18),
    legend.text = element_text(size=14),
    plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 17, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text( size=15, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),
    legend.position = c(0.7, 0.09)
  ) +
  coord_map()

# print(fatalities)
# 
# tikz(file="./Output/figures/map_fatalities.tex", standAlone=F,
#      width = 10, height = 8)
# print(fatalities)
# dev.off()

pdf(file="./Output/figures/map_fatalities.pdf", family="Palatino Linotype",
    width = 8, height = 8)
print(fatalities)
dev.off()

}


## Main analysis starts from here
## First, merge data with other data on assembly constituencies, using the dataset exported from Stata
{
assembly_data <- read.dta("./Data/_gen/constituency/const_data_r.dta")
complete_data <- merge(x=polyg, y=assembly_data, by.x="Map_ID", by.y="map_id")


complete_data@data$prop_sc <- complete_data@data$pc01_pca_p_sc/complete_data@data$pc01_pca_tot_p*100
complete_data@data$prop_schools <- complete_data@data$pc01_vd_p_sch/complete_data@data$pc01_pca_tot_p*100
complete_data@data$one <- complete_data@data$one.x
complete_data@data$acled_totalevents
complete_data@data$acled_edummy <- 0
complete_data@data$acled_edummy[complete_data@data$acled_totalevents!=0] <- 1
complete_data@data$acled_fdummy <- 0
complete_data@data$acled_fdummy[complete_data@data$fat!=0] <- 1


complete_data@data$sample_1[is.na(complete_data@data$sample_1)] <- 0
complete_data@data$sample_2[is.na(complete_data@data$sample_2)] <- 0
complete_data@data$sample_3[is.na(complete_data@data$sample_3)] <- 0
complete_data@data$sample_4[is.na(complete_data@data$sample_4)] <- 0
complete_data@data$sample_5[is.na(complete_data@data$sample_5)] <- 0


data_subsetted <- complete_data[complete_data@data$sample_2==1,]

## Only non-missing data - required manually for some of the spatial methods
data_subsetted <- data_subsetted[is.na(data_subsetted@data$acled_totalevents)==F & 
                                   is.na(data_subsetted@data$sc_prop_01_norm)==F & 
                                   is.na(data_subsetted@data$perc_sc)==F & 
                                   is.na(data_subsetted@data$pc01_pca_tot_p_norm)==F &
                                   is.na(data_subsetted@data$pc01_vd_p_sch_norm)==F &
                                   is.na(data_subsetted@data$pc01_vd_tar_road_norm)==F,]

}


## Stability plots
{

base_controls = c(
  'SC proportion' = "sc_prop_01_norm"
)
perm_controls = c(
  'Population' = 'log(pc01_pca_tot_p_norm)',
  '..             Primary Schools' = 'pc01_vd_p_sch_norm',
  '..           Middle Schools' = 'pc01_vd_m_sch_norm',
  "Literacy Rate" = "pc01_pca_p_lit_norm",
  "Roads" = "pc01_vd_tar_road_norm"  
)

nonperm_fe_controls = c(
  'State FE' = 'ST_CODE'
)

## CHECK STABILITY PLOT, WHY IS IT DIFFERENT FROM TABLE?
ols_events <- stability_plot(data = data_subsetted, 
               lhs = 'log(one+1)', 
               rhs = 'sc_reserved', 
               cluster = 'district_code',
              perm = perm_controls, 
               base = base_controls,
               nonperm_fe = nonperm_fe_controls,
               rel_height = 0.6,
               error_geom = 'errorbar',
              point_size = 2, # change the size of the coefficient points
              error_alpha = 0.5,
               coef_ylim = c(-0.5, 0.3), # change the endpoints of the y-axis
               control_text_size = 12,
              trim_top=4)

tikz(file="./Output/figures/events_stabilityplot.tex", standAlone=F, 
     width = 10, height = 6)
print(ols_events)
dev.off()


ols_fatalities <- stability_plot(data = data_subsetted, 
                             lhs = 'log(fat+1)', 
                             rhs = 'sc_reserved', 
                             cluster = 'district_code',
                             perm = perm_controls, 
                             base = base_controls,
                             nonperm_fe = nonperm_fe_controls,
                             point_size = 2, # change the size of the coefficient points
                             rel_height = 0.6,
                             error_alpha = 0.5,
                             error_geom = 'errorbar',
                             coef_ylim = c(-0.5, 0.3), # change the endpoints of the y-axis
                             control_text_size = 12,              
                             trim_top=4)



tikz(file="./Output/figures/fatalities_stabilityplot.tex", standAlone=F, 
     width = 10, height = 6)
print(ols_fatalities)
dev.off()
}


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
          title = "Constituency assemblies",
          covariate.labels = c("\\textbf{Seat reserved for SC}",
                               "Percent SC population",
                               "\\textbf{Reserved $\\times$ SC pop}",
                               "\\addlinespace Population in 1000",
                               "Primary Schools per 1000",
                               "Percent paved roads",
                               "Constant"),
          no.space=TRUE,
          column.labels   = c("log(Events+1)", "log(Fatalities+1)", "Events", "Fatalities"),
          column.separate = c(2, 2, 2, 2),
          model.names = T,
          dep.var.caption = "",
          dep.var.labels.include = F,
          omit.stat=c("ser", "theta", "f" ),
          notes = c("Results from regressions of conflict events/fatalities for constituency assemblies in the sample",
                    "as described in the main text.",
                    "Signficance levels: $^*10\\%, ^{**}5\\%, ^{***}1\\%$.\\label{tab:assembly}"),
          notes.align = "l",
          notes.append=F,
          notes.label = "\\textbf{Note:}",
          out="./Output/tables/assemblyreg.tex")


stargazer(ps11, ps1, ps21, ps2,  
          title = "Constituency assemblies",
          covariate.labels = c("\\textbf{Seat reserved for SC}",
                               "Percent SC population",
                               "\\textbf{Reserved $\\times$ SC pop}",
                               "\\addlinespace Population in 1000",
                               "Primary Schools per 1000",
                               "Percent paved roads",
                               "Constant"),
          no.space=TRUE,
          column.labels   = c("Events", "Fatalities"),
          column.separate = c(2, 2),
          model.names = T,
          dep.var.caption = "",
          dep.var.labels.include = F,
          omit.stat=c("ser", "theta", "f" ),
          notes = c("Results from Poisson regressions of conflict events and",
                    "fatalities for constituency assemblies in the sample.",
                    "Signficance levels: $^*10\\%, ^{**}5\\%, ^{***}1\\%$.\\label{tab:assemblyps}"),
          notes.align = "l",
          notes.append=F,
          notes.label = "\\textbf{Note:}",
          out="./Output/tables/assemblyreg_ps.tex")
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
    }  else if (y=="fdmodel") {
      tt <- c("Predicted Pr(conflict fatality)")
    }  else if (y=="emodel") {
      tt <- c("Predicted no. of conflict events")
    }  else if (y=="fmodel") {
      tt <- c("Predicted no. of fatalities")
    } else if (y=="logmodel") {
      tt <- c("Predicted no. of events")
    } else if (y=="logmodelf") {
      tt <- c("Predicted no. of fatalities")
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
    
    graph <-  ggplot(pr, aes(sc_prop_01_norm, p_events, group=sc_reserved))  +
      geom_ribbon(aes(ymin = LL, ymax = UL, fill=sc_reserved), alpha = c(0.4))  +
      geom_line(aes(color=sc_reserved), size = 1) +
      scale_color_manual(values=c("#c32750", "#002072"), labels=c("Unreserved", "Reserved")) +
      scale_fill_manual(values=c("#c32750", "#002072"), labels=c("Unreserved", "Reserved")) +
      labs(x = "Proportion SC in population", y = tt) +
      theme_minimal() +
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
tikz(file="./Output/figures/nb_events.tex", standAlone=F,
     width = 5, height = 4)
print(g1)
dev.off()
  

g2 <- make_prediction(df=data_subsetted, model="fmodel", type="nb")
tikz(file="./Output/figures/nb_fatalities.tex", standAlone=F,
     width = 5, height = 4)
print(g2)
dev.off()




g5 <- make_prediction(df=data_subsetted, model="emodel", type="ps")
tikz(file="./Output/figures/ps_events.tex", standAlone=F,
     width = 5, height = 4)
print(g5)
dev.off()


g6 <- make_prediction(df=data_subsetted, model="fmodel", type="ps")
tikz(file="./Output/figures/ps_fatalities.tex", standAlone=F,
     width = 5, height = 4)
print(g6)
dev.off()


  
g7 <- make_prediction(df=data_subsetted, model="logmodel", type="ols")
tikz(file="./Output/figures/log_events.tex", standAlone=F,
     width = 5, height = 4)
print(g7)
dev.off()


g8 <- make_prediction(df=data_subsetted, model="logmodelf", type="ols")
tikz(file="./Output/figures/log_fatalities.tex", standAlone=F,
     width = 5, height = 4)
print(g8)
dev.off()
}




# Generate spatial weight matrices
{
  nbpoly = poly2nb(polyg) # polygon to contiguity matrix
  nbpolymat = nb2mat(nbpoly) # normalized
  rownames(nbpolymat) = polyg$GEN
  colnames(nbpolymat) = polyg$GEN
  nbpoly_listw = mat2listw(nbpolymat) # Listw objets needed later!
  
  
  # CENTROIDS
  # Generating centroids of polygons
  cent = gCentroid(data_subsetted,byid=TRUE) # Polygonschwerpunkte
  
  # 10 nearest neighbors
  nn10 = knearneigh(cent, k=10)
  nb10 = knn2nb(nn10)
  nb10mat = nb2mat(nb10)
  nb10_listw = mat2listw(nb10mat)
  
  # Nearest neighbors by euclidian distance
  # Set d2! 
  d2 <- 200
  dnb = dnearneigh(cent , d1 = 0, d2 = d2)
  dnbmat = nb2mat(dnb)
  dnb_listw = mat2listw(dnbmat, style="W")
  text_dnb <- paste0("Euclidean Distance Matrix (up to ",d2,"km)")
  
  
  spatialmatrix <- dnb_listw # Normally: dnb
  graphtext <- text_dnb
data_subsetted$le <- data_subsetted$one + 1
data_subsetted$fe <- data_subsetted$fat + 1
  conflicts_lag <- lag.listw(spatialmatrix, data_subsetted$le) # Generate Spatial Lag
  fatalities_lag <- lag.listw(spatialmatrix, data_subsetted$fe) # Generate Spatial Lag
}


# Local Moran I statistics
{

  
  
  locm <- localmoran(data_subsetted$le, spatialmatrix)
  summary(locm) # Not all significant, but positive on average
  
  check <- data.frame(cbind(data_subsetted$le, conflicts_lag, data_subsetted$fe, fatalities_lag))
  names(check) <- c("Events", "Spatiallag", "fatalities", "flag")
  
  events_moran <- ggplot(check, 
                         aes(Events, Spatiallag)) + 
    geom_point(aes(), colour="red4") +
    theme_minimal() + 
    scale_x_continuous(trans='log2') + 
    scale_y_continuous(trans='log2') +
    geom_hline(yintercept=mean(check$Spatiallag), linetype="dotted", size=1) +
    geom_vline(xintercept=mean(check$Events), linetype="dotted", size=1) +
    xlab("Conflict Events") + ylab("Spatially lagged conflict events") +
    theme(legend.position="bottom", legend.title=element_blank()) + 
    theme(plot.title = element_text(size=18), plot.caption=element_text(size=12), text=element_text(size=16)) +
    stat_smooth(formula=y~x, se = T, method = lm , colour="grey20", linetype="solid", size=1)
  
  tikz(file="./Output/figures/events_moran.tex", standAlone=F,
       width = 7.5, height = 6)
  print(events_moran)
  dev.off()
  
  
  
  fatalities_moran <- ggplot(check, 
                             aes(fatalities, flag)) + 
    geom_point(aes(), colour="red4") +
    theme_minimal() + 
    scale_x_continuous(trans='log2') + 
    scale_y_continuous(trans='log2') +
    geom_hline(yintercept=mean(check$flag), linetype="dotted", size=1) +
    geom_vline(xintercept=mean(check$fatalities), linetype="dotted", size=1) +
    xlab("Fatalities") + ylab("Spatially lagged fatalities") +
    theme(legend.position="bottom", legend.title=element_blank()) + 
    theme(plot.title = element_text(size=18), plot.caption=element_text(size=12), text=element_text(size=16)) +
    stat_smooth(formula=y~x, se = T, method = lm , colour="grey20", linetype="solid", size=1)
  
  tikz(file="./Output/figures/fatalities_moran.tex", standAlone=F,
       width = 7.5, height = 6)
  print(fatalities_moran)
  dev.off()
  
}

## For spatial regressions
{
  # Global moran test
  #summary(basic_lm <- lm(g~ln_bippc+ln_p_e+ln_n_delta+ost, data=rmarkt_data))
  print(lm.morantest(ols11, spatialmatrix, alternative="two.sided"))
  print(lm.morantest(ps11, spatialmatrix, alternative="two.sided"))
  print(lm.morantest(nb11, spatialmatrix, alternative="two.sided"))
  
  
  # Don't do anything with direct neighbors - because we only use a subset of observations
  nbpoly = poly2nb(data_subsetted) # data_subsettedon to contiguity matrix
  nbpolymat = nb2mat(nbpoly, zero.policy = TRUE, style="W", glist=NULL) # normalized
  nbpoly_listw = mat2listw(nbpolymat) # Listw objets needed later!
  
  
  # CENTROIDS
  # Generating centroids of data
  cent = gCentroid(data_subsetted,byid=TRUE) # Polygonschwerpunkte
  
  # 5 nearest neighbors
  nn5 = knearneigh(cent, k=5)
  nb5 = knn2nb(nn5)
  nb5mat = nb2mat(nb5)
  nb5_listw = mat2listw(nb5mat, style="W")
  
  # 10 nearest neighbors
  nn10 = knearneigh(cent, k=10)
  nb10 = knn2nb(nn10)
  nb10mat = nb2mat(nb10)
  nb10_listw = mat2listw(nb10mat, style="W")
  
  # Nearest neighbors by euclidian distance
  
  # Set d2! 
  d2 <- 150
  dnb_150 = dnearneigh(cent , d1 = 0, d2 = d2)
  dnb_listw_150 = nb2listw(dnb_150, style="W")
  
  # Set d2! 
  d2 <- 200
  dnb_200 = dnearneigh(cent , d1 = 0, d2 = d2)
  dnb_listw_200 = nb2listw(dnb_200, style="W")
  
  # Set d2! 
  d2 <- 300
  dnb_300 = dnearneigh(cent , d1 = 0, d2 = d2)
  dnb_listw_300 = nb2listw(dnb_300, style="W")
  
  
  # Contiguity and inverse euclidean distances
  invlist_m1 <- lapply(nbdists(dnb_200, cent), function(x) 1/x)
  inv_listw_m1 <- nb2listw(dnb_200, glist=invlist_m1, style="W")
  
  # Contiguity and inverse euclidean distances
  invlist_m2 <- lapply(nbdists(dnb_200, cent), function(x) 1/x^2)
  inv_listw_m2 <- nb2listw(dnb_200, glist=invlist_m2, style="W")
  
  
  
  a <- lm.LMtests(basic_lm, nb5_listw, test="all")
  b <- lm.LMtests(basic_lm, nb10_listw, test="all")
  c <- lm.LMtests(basic_lm, dnb_listw_150, test="all")
  d <- lm.LMtests(basic_lm, dnb_listw_200, test="all")
  e <- lm.LMtests(basic_lm, dnb_listw_300, test="all")
  f <- lm.LMtests(basic_lm, inv_listw_m1, test="all")
  g <- lm.LMtests(basic_lm, inv_listw_m2, test="all")
  
  a1 <- cbind(a$LMerr$p.value, a$LMlag$p.value, a$RLMerr$p.value, a$RLMlag$p.value)
  b1 <- cbind(b$LMerr$p.value, b$LMlag$p.value, b$RLMerr$p.value, b$RLMlag$p.value)
  c1 <- cbind(c$LMerr$p.value, c$LMlag$p.value, c$RLMerr$p.value, c$RLMlag$p.value)
  d1 <- cbind(d$LMerr$p.value, d$LMlag$p.value, d$RLMerr$p.value, d$RLMlag$p.value)
  e1 <- cbind(e$LMerr$p.value, e$LMlag$p.value, e$RLMerr$p.value, e$RLMlag$p.value)
  f1 <- cbind(f$LMerr$p.value, f$LMlag$p.value, f$RLMerr$p.value, f$RLMlag$p.value)
  g1 <- cbind(g$LMerr$p.value, g$LMlag$p.value, g$RLMerr$p.value, g$RLMlag$p.value)
  
  
  
  names <- c(" ", "LMerr", "LMlag", "RLMerr", "RLMlag")
  table <- round(rbind(a1,b1,c1,d1,e1,f1,g1),4)
  table <- cbind(c("5 closest neighbors", 
                   "10 closest neighbors", 
                   "Neighbors up to 150km", 
                   "Neighbors up to 200km", 
                   "Neighbors up to 300km",
                   "Inverse distance up to 200km (x$^{-1}$)",
                   "Inverse distance up to 200km (x$^{-2}$)"), table)
  
  colnames(table) <- names
  
  table[as.numeric(table)<0.001] <- "<0.001"
  
  tab <- xtable(table, align="llcccc",caption = c("Tests for spatial dependence in linear models \\label{lmtest}"),
                digits=4)
  print(tab, type = "latex", booktabs = TRUE, digits=4, include.rownames=FALSE,
        sanitize.text.function=function(x){x},
        caption.placement="top",
        file = "./Output/tables/spatialtest.tex")
  
  rm(a, b, c, d, e, f, g,
     a1, b1, c1, d1, e1, f1, g1)
  
  
  
  
  ### Spatial regression
  spatialmodel <- logmodel1
  spatialmodel_f <- logmodelf1
  
  # Spatial autoregressive/lag model
  summary(sar_e <- lagsarlm(data=data_subsetted, formula=spatialmodel, dnb_listw_200)) 
  
  # Spatial Error Model
  summary(sem_e <- errorsarlm(data=data_subsetted, formula=spatialmodel, dnb_listw_200)) 
  
  # SLX Model
  summary(slx_e <- lmSLX(data=data_subsetted, formula=spatialmodel, dnb_listw_200)) 
  
  # Spatial Durbin Error Model
  summary(sdem_e <- errorsarlm(data=data_subsetted, formula=spatialmodel, dnb_listw_200, 
                               etype="emixed")) 
  
  # Spatial Durbin Model
  summary(sdm_e <- lagsarlm(data=data_subsetted, formula=spatialmodel, dnb_listw_200, 
                            type="mixed")) 
  
  # SARAR Model (Spatial Autoregressive with autoregressive error)
  summary(sarar_e <- sacsarlm(data=data_subsetted, formula=spatialmodel, dnb_listw_200, type="sac")) 
  
  # SARMA Model
  summary(sarma_e <- spautolm(data=data_subsetted, formula=spatialmodel, dnb_listw_200, family="SMA")) 
  
  
  # Manski Model
  summary(manski_e <- sacsarlm(data=data_subsetted, formula=spatialmodel, dnb_listw_200, type="sacmixed")) 
  
  #We can test if the Spatial Durbin Model can be restricted to the nested Spatial Error Model.
  e1 <- LR.sarlm(sdm_e, sem_e)
  e2 <- LR.sarlm(sdm_e, sar_e)
  e3 <- LR.sarlm(sdm_e, slx_e) 
  
  
  
  
  
  
  # Spatial autoregressive/lag model
  summary(sar_f <- lagsarlm(data=data_subsetted, formula=spatialmodel_f, dnb_listw_200)) 
  
  # Spatial Error Model
  summary(sem_f <- errorsarlm(data=data_subsetted, formula=spatialmodel_f, dnb_listw_200)) 
  
  # SLX Model
  summary(slx_f <- lmSLX(data=data_subsetted, formula=spatialmodel_f, dnb_listw_200)) 
  
  # Spatial Durbin Error Model
  summary(sdem_f <- errorsarlm(data=data_subsetted, formula=spatialmodel_f, dnb_listw_200, 
                               etype="emixed")) 
  
  # Spatial Durbin Model
  summary(sdm_f <- lagsarlm(data=data_subsetted, formula=spatialmodel_f, dnb_listw_200, 
                            type="mixed")) 
  
  # SARAR Model (Spatial Autoregressive with autoregressive error)
  summary(sarar_f <- sacsarlm(data=data_subsetted, formula=spatialmodel_f, dnb_listw_200, type="sac")) 
  
  # SARMA Model
  summary(sarma_f <- spautolm(data=data_subsetted, formula=spatialmodel_f, dnb_listw_200, family="SMA")) 
  
  
  # Manski Model
  summary(manski_f <- sacsarlm(data=data_subsetted, formula=spatialmodel_f, dnb_listw_200, type="sacmixed")) 
  
  #We can test if the Spatial Durbin Model can be restricted to the nested Spatial Error Model.
  f1 <- LR.sarlm(sdm_f, sem_f)
  f2 <- LR.sarlm(sdm_f, sar_f)
  f3 <- LR.sarlm(sdm_f, slx_f) 
  
  
  
  
  # All have p-values far below 0.001. Model cannot be restricted. Spatial Durbin Models should be used!
  stargazer(sdm_e, sdem_e, sdm_f, sdem_f,
            title = "Results from spatial regressions",
            covariate.labels = c("\\textbf{Seat reserved for SC}",
                                 "Percent SC population",
                                 "Total population in 1000",
                                 "Schools per 1000 population",
                                 "Percentage paved roads",
                                 "\\textbf{Lag (seat reserved)}",
                                 "Lag (percent SC pop)",
                                 "Lag (population)",
                                 "Lag (schools)",
                                 "Lag (roads)"),
            dep.var.caption = "Spatial models",
            no.space=TRUE,
            dep.var.labels = c("Log (Conflict events)", "Log (Conflict fatalities)"),
            out="./Output/tables/spatialreg.tex")
  
  
  
  
  
  
  
  ## Table
  events_i <- summary(i1 <- impacts(sdm_e, listw=dnb_listw_200, R=5), zstats=T)
  fatalities_i <- summary(i1 <- impacts(sdm_f, listw=dnb_listw_200, R=5), zstats=T)
  
  # Function Outputtable: The function extracts the coefficients and significance levels from the impacts with simulated standard errors. 
  outputtable <- function(x) {
    a <- x
    
    direct <- a$res$direct
    indirect <- a$res$indirect
    total <- a$res$total
    
    direct_sd <- a$direct_sum$statistics[,2]
    indirect_sd <- a$indirect_sum$statistics[,2]
    total_sd <- a$total_sum$statistics[,2]
    
    direct_p <- a$pzmat[,1]
    indirect_p <- a$pzmat[,2]
    total_p <- a$pzmat[,3]
    
    impacts_splm_1 <- cbind(direct, indirect, total)
    impact_sd_splm_1 <- cbind(direct_sd, indirect_sd, total_sd)
    impact_p_splm_1 <- cbind(direct_p, indirect_p, total_p)
    
    for (i in 1:dim(impacts_splm_1)[1]) {
      a <- i
      if (i==1) {
        def <- rbind(round(impacts_splm_1[1,], 3), paste0("(",round(impact_sd_splm_1[1,],3),")" ))
      }
      else {
        def <- rbind(def, round(impacts_splm_1[a,],3), paste0("(",round(impact_sd_splm_1[a,],3),")"))  
      }
    }
    rownames(def) <- c(rep("", dim(def)[1]))
    for (i in 1:dim(impact_sd_splm_1)[1]){
      rownames(def)[i*2-1] <- rownames(impact_sd_splm_1)[i]
    }
    
    
    for (i in 1:dim(impacts_splm_1)[1]) {
      a <- i
      if (i==1) {
        pvals <- rbind(impact_p_splm_1[1,], c(rep("", dim(impact_p_splm_1)[2])))
      }
      else {
        pvals <- rbind(pvals, impact_p_splm_1[a,], c(rep("", dim(impact_p_splm_1)[2])))
      }
    }
    
    pvals <- matrix(as.numeric(pvals), ncol=dim(pvals)[2])
    stars <- symnum(pvals, corr = FALSE, cutpoints = c(0,  .01,.05,.1, 1), na="",
                    symbols = c('$^{***}$','$^{**}$',"$^{*}$"," ")) # significance levels
    for(i in 1:dim(def)[2]) {
      def[,i] <- paste0(def[,i],as.character(stars[,i]))
    }
    
    colnames(def) <- c("Direct", "Indirect", "Total")
    def
  }
  
  names_temp <- c("SC reserved", " ", "Log of percentage of SC", " ", "Population in 1000", " ")
  colnames_temp <- c(" ", "Direct", "Indirect", "Total")
  panel_impacts <- xtable(rbind(colnames_temp, cbind(names_temp, outputtable(events_i))),
                          caption = c("Effects of variables in spatial models \\label{spatial}"),
                          digits=4)
  
  ## align?
  print(panel_impacts, 
        file="./Output/tables/impacts.tex",
        booktabs=T, type="latex",
        hline.after = c(-1,1,nrow(panel_impacts)),
        include.rownames=FALSE,
        include.colnames=FALSE,
        caption.placement="top",
        sanitize.text.function=function(x){x})
}



## Bootstrap
{
  make_graph <- function(model) {
    
    x <- data_subsetted@data
    pr <- data.frame(
      sc_prop_01_norm = rep(seq(from = quantile(x$sc_prop_01_norm, na.rm=T, probs=0.1),
                                to = quantile(x$sc_prop_01_norm, na.rm=T, probs=0.9),
                                length.out = 100), 2),
      sc_reserved = factor(rep(0:1, each = 100), levels = 0:1, labels =
                             levels(as.factor(x$sc_reserved)))
    )
    pr$pc01_pca_tot_p_norm = mean(x$pc01_pca_tot_p_norm, na.rm=T)
    pr$pc01_vd_p_sch_norm = mean(x$pc01_vd_p_sch_norm, na.rm=T)
    pr$pc01_vd_tar_road_norm = mean(x$pc01_vd_tar_road_norm, na.rm=T)
    pr$sc_reserved <- as.numeric(pr$sc_reserved)-1
    pr$sc_reserved_mean <- mean(as.numeric(x$sc_reserved-1), na.rm=T)
    pr <- cbind(pr, bootest)
    
    pr$sc_reserved <- as.factor(pr$sc_reserved)
    if (model=="events") {
      tt <- c("Predicted no. of conflict events")
    }
    else {
      tt <- c("Predicted no. of conflict fatalities")
    }
    
    
    graph <-  ggplot(pr, aes(sc_prop_01_norm, fit, group=sc_reserved))  +
      geom_ribbon(aes(ymin = LL, ymax = UL, fill=sc_reserved), alpha = .25)  +
      geom_line(aes(color=sc_reserved), size = 1) +
      scale_color_manual(values=c("#c32750", "#002072"), labels=c("Unreserved", "Reserved")) +
      scale_fill_manual(values=c("#c32750", "#002072"), labels=c("Unreserved", "Reserved")) +
      labs(x = "Proportion SC in population", y = tt) +
      theme_minimal() +
      theme(legend.text = element_text(margin = margin(r = 30, unit = "pt"))) +
      theme(panel.grid.minor = element_blank()) +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(legend.position="bottom", legend.title=element_blank(),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            axis.title.y = element_text(size = 12),
            axis.title.x = element_text(size = 12),
            legend.text = element_text(size = 12))
  }
  
  
  
  
  
  statols1 <- function(df, inds) {
    summary( zinf <- lm(formula = log(acled_totalevents+1) ~ sc_reserved + sc_prop_01_norm +  I(sc_reserved*sc_prop_01_norm) +
                          pc01_pca_tot_p_norm +
                          pc01_vd_p_sch_norm + 
                          pc01_vd_tar_road_norm , data=df[inds, ])) 
    x <- df
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
    
    predict(object = zinf, newdata = pr, type = "response", se.fit = F)
  }
  
  
  
  
  
  statols2 <- function(df, inds) {
    
    summary( zinf <- lm(formula = log(fat+1) ~ sc_reserved + sc_prop_01_norm +  I(sc_reserved*sc_prop_01_norm) +
                          pc01_pca_tot_p_norm +
                          pc01_vd_p_sch_norm + 
                          pc01_vd_tar_road_norm , data=df[inds, ])) 
    x <- df
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
    
    predict(object = zinf, newdata = pr, type = "response", se.fit = F)
  }
  
  
  
  
  
  
  
  statps1 <- function(df, inds) {
    model <- formula(acled_totalevents ~ sc_reserved + sc_prop_01_norm + I(sc_reserved*sc_prop_01_norm) +  
                       pc01_pca_tot_p_norm +
                       pc01_vd_p_sch_norm + 
                       pc01_vd_tar_road_norm  | sc_reserved + sc_prop_01_norm + I(sc_reserved*sc_prop_01_norm) +  
                       pc01_pca_tot_p_norm +
                       pc01_vd_p_sch_norm + 
                       pc01_vd_tar_road_norm)
    
    fit.hurdle <- pscl::hurdle(model, dist="poisson", data=df[inds, ])
    zinf <- pscl::zeroinfl(model, dist="poisson",
                           data=df[inds, ],
                           start=fit.hurdle$coefficients)
    
    x <- df
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
    
    predict(object = zinf, newdata = pr, type = "response", se.fit = F)
  }
  
  
  
  
  
  
  statps2 <- function(df, inds) {
    model <- formula(fat ~ sc_reserved + sc_prop_01_norm + I(sc_reserved*sc_prop_01_norm) +  
                       pc01_pca_tot_p_norm +
                       pc01_vd_p_sch_norm + 
                       pc01_vd_tar_road_norm  | sc_reserved + sc_prop_01_norm + I(sc_reserved*sc_prop_01_norm) +  
                       pc01_pca_tot_p_norm +
                       pc01_vd_p_sch_norm + 
                       pc01_vd_tar_road_norm)
    
    fit.hurdle <- pscl::hurdle(model, dist="poisson", data=df[inds, ])
    zinf <- pscl::zeroinfl(model, dist="poisson",
                           data=df[inds, ],
                           start=fit.hurdle$coefficients)
    
    x <- df
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
    
    predict(object = zinf, newdata = pr, type = "response", se.fit = F)
  }
  
  
  
  
  
  
  
  
  
  
  statnb1 <- function(df, inds) {
    model <- formula(acled_totalevents ~ sc_reserved + sc_prop_01_norm + I(sc_reserved*sc_prop_01_norm) +  
                       pc01_pca_tot_p_norm +
                       pc01_vd_p_sch_norm + 
                       pc01_vd_tar_road_norm  | sc_reserved + sc_prop_01_norm + I(sc_reserved*sc_prop_01_norm) +  
                       pc01_pca_tot_p_norm +
                       pc01_vd_p_sch_norm + 
                       pc01_vd_tar_road_norm)
    
    fit.hurdle <- pscl::hurdle(model, dist="negbin", data=df[inds, ])
    zinf <- pscl::zeroinfl(model, dist="negbin",
                           data=df[inds, ],
                           start=fit.hurdle$coefficients)
    
    x <- df
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
    
    predict(object = zinf, newdata = pr, type = "response", se.fit = F)
  }
  
  
  
  
  
  
  statnb2 <- function(df, inds) {
    model <- formula(fat ~ sc_reserved + sc_prop_01_norm + I(sc_reserved*sc_prop_01_norm) +  
                       pc01_pca_tot_p_norm +
                       pc01_vd_p_sch_norm + 
                       pc01_vd_tar_road_norm  | sc_reserved + sc_prop_01_norm + I(sc_reserved*sc_prop_01_norm) +  
                       pc01_pca_tot_p_norm +
                       pc01_vd_p_sch_norm + 
                       pc01_vd_tar_road_norm)
    
    fit.hurdle <- pscl::hurdle(model, dist="negbin", data=df[inds, ])
    zinf <- pscl::zeroinfl(model, dist="negbin",
                           data=df[inds, ],
                           start=fit.hurdle$coefficients)
    
    x <- df
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
    
    predict(object = zinf, newdata = pr, type = "response", se.fit = F)
  }
  
  
  
  
  
  
  set.seed(2018)
  res <- boot(data=data_subsetted@data, statistic=statols1, R = 5000, parallel = "snow", ncpus = 4)
  
  bootci <- lapply(1:200, function(x) {
    li <-  exp(quantile(res$t[,x],0.025))-1
    ui <- exp(quantile(res$t[,x],0.975))-1
    ci <- rbind(li,ui)
  })
  
  bootci_df <- data.frame(matrix(unlist(bootci), ncol=2, byrow=TRUE))
  bootest <- cbind(exp(res$t0)-1, bootci_df)
  colnames(bootest) <- c("fit", "LL", "UL")
  
  g1 <- make_graph(model="events")
  
  
  
  
  set.seed(2018)
  res <- boot(data=data_subsetted@data, statistic=statols2, R = 5000, parallel = "snow", ncpus = 4)
  
  bootci <- lapply(1:200, function(x) {
    li <-  exp(quantile(res$t[,x],0.025))-1
    ui <- exp(quantile(res$t[,x],0.975))-1
    ci <- rbind(li,ui)
  })
  
  bootci_df <- data.frame(matrix(unlist(bootci), ncol=2, byrow=TRUE))
  bootest <- cbind(exp(res$t0)-1, bootci_df)
  colnames(bootest) <- c("fit", "LL", "UL")
  
  g2 <- make_graph(model="fatalities")
  
  
  
  
  
  set.seed(2018)
  res <- boot(data=data_subsetted@data, statistic=statps1, R = 5000, parallel = "snow", ncpus = 4)
  
  bootci <- lapply(1:200, function(x) {
    li <-  quantile(res$t[,x],0.025)
    ui <- quantile(res$t[,x],0.975)
    ci <- rbind(li,ui)
  })
  
  bootci_df <- data.frame(matrix(unlist(bootci), ncol=2, byrow=TRUE))
  bootest <- cbind(res$t0, bootci_df)
  colnames(bootest) <- c("fit", "LL", "UL")
  
  g3 <- make_graph(model="events")
  
  
  
  set.seed(2018)
  res <- boot(data=data_subsetted@data, statistic=statps2, R = 5000, parallel = "snow", ncpus = 4)
  
  bootci <- lapply(1:200, function(x) {
    li <-  quantile(res$t[,x],0.025)
    ui <- quantile(res$t[,x],0.975)
    ci <- rbind(li,ui)
  })
  
  bootci_df <- data.frame(matrix(unlist(bootci), ncol=2, byrow=TRUE))
  bootest <- cbind(res$t0, bootci_df)
  colnames(bootest) <- c("fit", "LL", "UL")
  
  g4 <- make_graph(model="fatalities")
  
  
  
  
  
  
  set.seed(2018)
  res <- boot(data=data_subsetted@data, statistic=statnb1, R = 5000, parallel = "snow", ncpus = 4)
  
  bootci <- lapply(1:200, function(x) {
    li <-  quantile(res$t[,x],0.025)
    ui <- quantile(res$t[,x],0.975)
    ci <- rbind(li,ui)
  })
  
  bootci_df <- data.frame(matrix(unlist(bootci), ncol=2, byrow=TRUE))
  bootest <- cbind(res$t0, bootci_df)
  colnames(bootest) <- c("fit", "LL", "UL")
  
  g5 <- make_graph(model="events")
  
  
  
  set.seed(2018)
  res <- boot(data=data_subsetted@data, statistic=statnb2, R = 5000, parallel = "snow", ncpus = 4)
  
  bootci <- lapply(1:200, function(x) {
    li <-  quantile(res$t[,x],0.025)
    ui <- quantile(res$t[,x],0.975)
    ci <- rbind(li,ui)
  })
  
  bootci_df <- data.frame(matrix(unlist(bootci), ncol=2, byrow=TRUE))
  bootest <- cbind(res$t0, bootci_df)
  colnames(bootest) <- c("fit", "LL", "UL")
  
  g6 <- make_graph(model="fatalities")
  
  
  
  
  tikz(file="./Output/figures/log_events_boot.tex", standAlone=F,
       width = 5, height = 4)
  print(g1)
  dev.off()
  
  
  tikz(file="./Output/figures/log_fatalities_boot.tex", standAlone=F,
       width = 5, height = 4)
  print(g2)
  dev.off()
  
  
  tikz(file="./Output/figures/nb_events_zinfl.tex", standAlone=F,
       width = 5, height = 4)
  print(g3)
  dev.off()
  
  tikz(file="./Output/figures/nb_fatalities_zinfl.tex", standAlone=F,
       width = 5, height = 4)
  print(g4)
  dev.off()
  
  tikz(file="./Output/figures/ps_events_zinfl.tex", standAlone=F,
       width = 5, height = 4)
  print(g5)
  dev.off()
  
  tikz(file="./Output/figures/ps_fatalities_zinfl.tex", standAlone=F,
       width = 5, height = 4)
  print(g6)
  dev.off()
  
  
}











