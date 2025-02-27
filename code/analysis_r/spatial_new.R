## Spatial Analysis for Extended Essay
setwd("~/Library/CloudStorage/GoogleDrive-jonathan_old@berkeley.edu/My Drive/_Berkeley Research/Reservations and Conflict/")
getwd()

#==============
# LOAD PACKAGES
#==============


install.packages("devtools")
library(devtools)
install_github("markwestcott34/stargazer-booktabs")
detach("package:stargazer", unload=TRUE)
library("stargazer")

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


mapdata$events_d = cut(mapdata$one,breaks=c(-0.1,0,1,2,5,10,50,2000),labels=F,  ordered_result=T)
mapdata$events_d <- factor(mapdata$events_d)


events <- ggplot(mapdata, aes( x = long, y = lat, group = group )) +
  theme_void() +
  geom_polygon(aes(fill=events_d)) +
  scale_fill_manual(
    values=c("#0a6264","#428f87","#86bca6","#e6e2bd","#ecb176","#e77f44","#db4324"),
    labels=c("0", "1", "2", "3-5", "6-10", "11-50", "51+", ""),
    name="",
    guide = guide_legend(
      keyheight = unit(3, units = "mm"),
      keywidth=unit(12, units = "mm"),
      label.position = "bottom",
      title.position = 'top', nrow=1)
  ) +
  #scale_fill_viridis(trans = "log", breaks=c(0,0.1,1,2,5,10,50,300), labels=c("0", "0", "1", "2", "5", "10", "50", "300+"), name="", guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1) ) +
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



ggsave("./Output/figures/map_events.pdf", family="Palatino Linotype",
       events,
       width = 8, height = 8,
       device = cairo_pdf)





mapdata$fatalities_d = cut(mapdata$fat,breaks=c(-0.1,0,1,2,5,10,20,2000),labels=F,  ordered_result=T)
mapdata$fatalities_d <- factor(mapdata$fatalities_d)


mapdata$fatalities <- mapdata$fat + 0.05

fatalities <- ggplot(mapdata, aes( x = long, y = lat, group = group )) +
  theme_void() +
  geom_polygon(aes(fill=fatalities_d)) +
  scale_fill_manual(
    values=c("#0a6264","#428f87","#86bca6","#e6e2bd","#ecb176","#e77f44","#db4324"),
    labels=c("0", "1", "2", "3-5", "6-10", "11-20", "21+"),
    name="",
    guide = guide_legend(
      keyheight = unit(3, units = "mm"),
      keywidth=unit(12, units = "mm"),
      label.position = "bottom",
      title.position = 'top', nrow=1)
  ) +
 # scale_fill_viridis(trans = "log", breaks=c(0,0.1,1,2,5,10,20,100), labels=c("0", "0", "1", "2", "5", "10", "20", "100+"), name="", guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1) ) +
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

ggsave("./Output/figures/map_fatalities.pdf", family="Palatino Linotype",
       fatalities,
       width = 8, height = 8,
       device = cairo_pdf)








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

## NEW MAPS
{# New maps
  
  
  complete_data@data$sc_pop_d = cut(complete_data@data$perc_sc  , breaks=c(-0.1, 5, 10, 15, 20, 30, 40, 100), labels=F,  ordered_result=T)
  complete_data@data$sc_pop_d[is.na(complete_data@data$sc_pop_d)] <- 8
  complete_data@data$sc_pop_d = factor( complete_data@data$sc_pop_d )

  
  mapdata2 <- tidy(complete_data, region = "Map_ID")
  mapdata2 <- merge(x=mapdata2, y=complete_data@data, by.x="id", by.y="Map_ID")
  mapdata2$sc_pop_d[is.na(  mapdata2$sc_pop_d)] <- 8
  
  scmap <- ggplot( mapdata2, aes( x = long, y = lat, group = group )) +
    theme_void() +
    geom_polygon(aes(fill=sc_pop_d)) +
    scale_fill_manual(
      values=c("#0a6264","#428f87","#86bca6","#e6e2bd","#ecb176","#e77f44","#db4324", "#777777"),
      labels=c("0-5", "5-10", "10-15", "15-20", "20-30", "30-40", "40+", "NA"),
      name="",
      guide = guide_legend(
        keyheight = unit(3, units = "mm"),
        keywidth=unit(12, units = "mm"),
        label.position = "bottom",
        title.position = 'top', nrow=1)
    ) +
    #scale_fill_viridis(trans = "log", breaks=c(0,0.1,1,2,5,10,50,300), labels=c("0", "0", "1", "2", "5", "10", "50", "300+"), name="", guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1) ) +
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
  
  print(scmap)
  
  ggsave("./Output/figures/map_sc.pdf", family="Palatino Linotype",
         scmap,
         width = 8, height = 8,
         device = cairo_pdf)
  
  
  mapdata2$reservestatus <- 5
  mapdata2$reservestatus[mapdata2$sc_reserved==1] <- 3
  mapdata2$reservestatus[mapdata2$sc_sample_2==1] <- 4
  mapdata2$reservestatus[mapdata2$sc_reserved==0] <- 2
  mapdata2$reservestatus[mapdata2$runner_up_sc_2==1] <- 1

  mapdata2$reservestatus <- factor( mapdata2$reservestatus)
  
  library(stringr)
  
  reservemap <- ggplot( mapdata2, aes( x = long, y = lat, group = group )) +
    theme_void() +
    geom_polygon(aes(fill=reservestatus)) +
    scale_fill_manual(
      values=c("#0a6264","#86bca6","#ecb176","#db4324", "#777777"),
      labels=c("Unreserved\nSample", "Unreserved\nNot in sample", "Reserved\nNot in sample", "Reserved\n Sample", "NA"),
      name="",
      guide = guide_legend(
        keyheight = unit(3, units = "mm"),
        keywidth=unit(12, units = "mm"),
        label.position = "bottom",
        title.position = 'top', nrow=1)
    ) +
    #scale_fill_viridis(trans = "log", breaks=c(0,0.1,1,2,5,10,50,300), labels=c("0", "0", "1", "2", "5", "10", "50", "300+"), name="", guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1) ) +
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
  
  print(reservemap)
  
  
  ggsave("./Output/figures/map_reserve.pdf", family="Palatino Linotype",
         reservemap,
         width = 8, height = 8,
         device = cairo_pdf)
}






  ## SET SAMPLE HERE!
data_subsetted <- complete_data[complete_data@data$sample_2==1,]

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
               rel_height = 0.6,
               error_geom = 'errorbar',
              point_size = 3, # change the size of the coefficient points
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
                             point_size = 3, # change the size of the coefficient points
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
  
  
summary(nb1 <- glm.nb(formula= acled_totalevents ~ sc_reserved + sc_prop_01_norm + I(sc_reserved*sc_prop_01_norm) +  
                        pc01_pca_tot_p_norm + 
                        pc01_vd_p_sch_norm + 
                        pc01_vd_tar_road_norm, 
                     data=data_subsetted))
summary(nb2 <- glm.nb(formula=fat ~ sc_reserved + sc_prop_01_norm +  I(sc_reserved*sc_prop_01_norm) +  
                        pc01_pca_tot_p_norm +
                        pc01_vd_p_sch_norm + 
                        pc01_vd_tar_road_norm  ,
                      data=data_subsetted))
summary(ps1 <- glm(formula= acled_totalevents ~ sc_reserved + sc_prop_01_norm + I(sc_reserved*sc_prop_01_norm) +  
                     pc01_pca_tot_p_norm + 
                     pc01_vd_p_sch_norm + 
                     pc01_vd_tar_road_norm  ,
                      data=data_subsetted, family="poisson"))
summary(ps2 <- glm(formula=fat ~ sc_reserved + sc_prop_01_norm +  I(sc_reserved*sc_prop_01_norm) +  
                     pc01_pca_tot_p_norm +
                     pc01_vd_p_sch_norm + 
                     pc01_vd_tar_road_norm ,
                      data=data_subsetted, family="poisson"))
summary(ols1 <- lm(formula = log(acled_totalevents+1) ~ sc_reserved + sc_prop_01_norm +  I(sc_reserved*sc_prop_01_norm) +
                     pc01_pca_tot_p_norm + 
                     pc01_vd_p_sch_norm + 
                     pc01_vd_tar_road_norm  ,
                   data=data_subsetted))
summary(ols2 <- lm(formula = log(fat+1) ~ sc_reserved + sc_prop_01_norm +  I(sc_reserved*sc_prop_01_norm) +
                     pc01_pca_tot_p_norm +
                     pc01_vd_p_sch_norm + 
                     pc01_vd_tar_road_norm  ,
                   data=data_subsetted))

summary(nb11 <- glm.nb(formula=acled_totalevents ~ sc_reserved + sc_prop_01_norm +  
                         pc01_pca_tot_p_norm +
                         pc01_vd_p_sch_norm + 
                         pc01_vd_tar_road_norm + factor(DIST_NAME) ,
                      data=data_subsetted))
summary(nb21 <- glm.nb(formula=fat ~ sc_reserved + sc_prop_01_norm +  
                         pc01_pca_tot_p_norm +
                         pc01_vd_p_sch_norm + 
                         pc01_vd_tar_road_norm + factor(DIST_NAME) ,
                      data=data_subsetted))
summary(ps11 <- glm(formula=acled_totalevents ~ sc_reserved + sc_prop_01_norm +  
                      pc01_pca_tot_p_norm +
                      pc01_vd_p_sch_norm + 
                      pc01_vd_tar_road_norm + factor(DIST_NAME),
                   data=data_subsetted, family="poisson"))
summary(ps21 <- glm(formula=fat ~ sc_reserved + sc_prop_01_norm +  
                      pc01_pca_tot_p_norm +
                      pc01_vd_p_sch_norm + 
                      pc01_vd_tar_road_norm + factor(DIST_NAME) ,
                   data=data_subsetted, family="poisson"))
summary(ols11 <- lm(formula = log(acled_totalevents+1) ~ sc_reserved + sc_prop_01_norm +  
                      pc01_pca_tot_p_norm + 
                      pc01_vd_p_sch_norm + 
                      pc01_vd_tar_road_norm ,
                   data=data_subsetted))
summary(ols21 <- lm(formula = log(fat+1) ~ sc_reserved + sc_prop_01_norm +  
                      pc01_pca_tot_p_norm +
                      pc01_vd_p_sch_norm + 
                      pc01_vd_tar_road_norm + factor(DIST_NAME),
                   data=data_subsetted))

summary(data_subsetted$acled_totalevents)
mean(data_subsetted@data$acled_totalevents)
var(data_subsetted@data$acled_totalevents)

mean(data_subsetted@data$fat)
var(data_subsetted@data$fat)


stargazer(ols11, ols21,  ps11, ps21, nb11,  nb21, 
          keep = c('sc_reserved', 'sc_prop_01_norm','Constant'),
          covariate.labels = c("\\textbf{Seat reserved for SC}",
                               "Percent SC population", "Constant"),
          no.space=TRUE,
          column.labels   = c("OLS", "Poisson", "Negative Binomial"),
          column.separate = c(2, 2, 2),
          model.names = F,
          dep.var.caption = "",
          dep.var.labels.include = F,
          omit.stat=c("ser", "theta", "f" , "adj.rsq","ll", "aic"),
          out="./Output/tables/assembly_slides_2023.tex")


stargazer(ps11, ps1, ps21, ps2,  
          title = "Robustness Check: Poisson regressions for constituency assemblies",
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
                    "Significance levels: $^*10\\%, ^{**}5\\%, ^{***}1\\%$.\\label{tab:assemblyps}"),
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
      lim <- c(0,7)
      
    }
    else {
      tt <- c("Predicted no. of conflict fatalities")
      lim <- c(0,0.7)
      
    }
    
    
    graph <-  ggplot(pr, aes(sc_prop_01_norm*v1+m1, fit, group=sc_reserved))  +
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

