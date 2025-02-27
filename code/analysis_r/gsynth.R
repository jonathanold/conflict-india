## This code performs the GSynth-Analysis and produces the graphs for
## the paper (figure 5)
## The control variables included are the same as in the TSCS-regressions
## and the event studies. Various robustness checks are available in the other files.

## Set the working directory and then execute the code.

##rm(list=ls())
## detach("package:gsynth", unload=TRUE)
## remove.packages("gsynth")
## .rs.restartR()

setwd("~/Library/CloudStorage/GoogleDrive-jonathan_old@berkeley.edu/My Drive/_Berkeley Research/Reservations and Conflict/")
# install.packages('devtools', repos = 'http://cran.us.r-project.org') # if not already installed
## for processing C++ code
# install.packages("Rcpp") 
## for plotting
require(ggplot2)
library(tikzDevice)

## require(GGally) 
## for parallel computing 
require(foreach)  
require(doParallel) 
require(abind) 
## install.packages("RcppArmadillo")
library("RcppArmadillo")
library("Rcpp")
# install.packages("gsynth")
## devtools::install_github('xuyiqing/gsynth')
# install.packages('gsynth', type = 'source')

library(panelView)
##### UNCOMMENT THIS in case you do need our version of GSynth
##  install.packages("gtable")
## install.packages("remotes")
##  library(remotes)
##  remotes::install_github("r-lib/gtable")
## library(gtable)

## ## devtools::install_local("./Software/gsynth-master", force=T) ##graph or new or vdem


library(gsynth)
library(foreign)



root <- paste0("./Output/gsynth/")

## Rich countries c(21,20,19,17,14))
mydata <- read.dta("./Data/_gen/state/state_dataset.dta")

##rich_countries <- gsynth(repress_fariss ~ treat_full + "[[gdp]]" + "[[population]]"    + conflictdummy  +  pop_youthbulge +  "[[trade]]",



 tikz(file="./Output/figures/state_treat.tex", standAlone=F,
      width = 7.5, height = 4.5)
panelView(id ~ treat , 
          data=mydata[mydata$year>=1970,],index = c("state_name","year"),
          pre.post=F,
           na.rm=T,
          xlab ="",
          ylab = "",
          main = "",
          gridOff = F,
         color=c("#5e5e5e60", "#8faff7", "#06266F"),
         legend.labs = c("Missing", "Before", "After"),
          axis.lab.gap=c(4,0),
          background="white",
          cex.axis=11)
dev.off()




gsynth <- gsynth(lriot ~ treat  ,
                         data=mydata, 
                         seed=1234,
                         na.rm=T,
                         index = c("code","year"), 
                         force = "time",
                         CV=T, 
                         r = c(0,5), 
                         se = TRUE, 
                         alpha=0.1,
                         estimator="mc",
                         nboots=100, # 500, #set this higher in future specifications
                         min.T0=5, # fits to our definition
                         parallel = TRUE, 
                         cores=8)



plot(gsynth, type="gap", main="", ylim=c(-3,3))



i <- length(rich_countries$id.tr)

zy <- paste0(root,"educated.tex")
ab <- paste0(root,"educated.pdf")
zz <- paste0(root,"educated_ctf.tex")


# out <- list(rich_countries, i)
# saveRDS(out, file = paste0(root,"rich_",i,"_.rds"))

tikz(file=zy, standAlone=F,
     width = 7, height = 5.2)
plot(gsynth, type="gap", main="")
dev.off()

pdf(file=ab, 
    width = 7, height = 5.2)
plot(rich_countries, type="gap", main="School years at transition above mean")
dev.off()

tikz(file=zz, standAlone=F,
     width = 7, height = 5.2)
plot(rich_countries,  type = "counterfactual", raw = "none", main="School years at transition above mean")
dev.off()












## Rich countries c(21,20,19,17,14))
z <- paste("./Data/_Generated/Splits/",folder,"Rich/gsynth_not_educated.dta", sep="")
mydata <- read.dta(z)

##rich_countries <- gsynth(repress_fariss ~ treat_full + "[[gdp]]" + "[[population]]"    + conflictdummy  +  pop_youthbulge +  "[[trade]]",
rich_countries <- gsynth(repress_fariss ~ treat_full + "X=conflictdummy + lgdppc + pop_youthbulge + wdi_trade" ,
                         data=mydata, 
                         seed=1234,
                         na.rm=T,
                         index = c("ctry","year"), 
                         force = "time",
                         CV=T, 
                         EM=T,
                         r = c(0,5), 
                         se = TRUE, 
                         alpha=0.1,
                         estimator="mc",
                         nboots=1000, # 500, #set this higher in future specifications
                         min.T0=10, # fits to our definition
                         parallel = TRUE, 
                         cores=8)

i <- length(rich_countries$id.tr)

zy <- paste0(root,"uneducated.tex")
ab <- paste0(root,"uneducated.pdf")
zz <- paste0(root,"uneducated_ctf.tex")


# out <- list(rich_countries, i)
# saveRDS(out, file = paste0(root,"rich_",i,"_.rds"))

tikz(file=zy, standAlone=F,
     width = 7, height = 5.2)
plot(rich_countries, type="gap", main="")
dev.off()

pdf(file=ab, 
    width = 7, height = 5.2)
plot(rich_countries, type="gap", main="School years at transition below mean")
dev.off()

tikz(file=zz, standAlone=F,
     width = 7, height = 5.2)
plot(rich_countries,  type = "counterfactual", raw = "none", main="School years at transition below mean")
dev.off()






cat("\\begin{figure}[h!]\n \\resizebox{1\\textwidth}{!}{", file ="./Output/figures/state_treat.tex", append=F)
sink("./Output/figures/state_treat.tex", append=TRUE)

sink()
cat("} \\caption{Staggggered introduction of reservations} \n \\label{fig:treatplot}\n", file = "./Output/figures/state_treat.tex", append=TRUE)
cat("\\end{figure}\n", file = "./Output/figures/state_treat.tex", append=TRUE)






