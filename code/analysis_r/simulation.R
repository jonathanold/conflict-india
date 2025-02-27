# Questions:
# Subtract time trend of control group for all groups?
# Are fixed effects correlated with treatment assignment?


### Diff in Diff
library(plm)
library(ggplot2)
library(tikzDevice)



rm(list=ls())

setwd("~/Library/CloudStorage/GoogleDrive-jonathan_old@berkeley.edu/My Drive/_Berkeley Research/Reservations and Conflict/Output/figures/")

set.seed(123571113)

expand_r<-function(x,n) rep(x,replace(n,n<1,1))

cutoff1 <- 1/5
cutoff2 <- 4/5
n <- 10^4

## Monte Carlo function is hiding here
simulate_did <- function(i) {
# 1000 villages
data <- seq(1:1000)
# Random variables for fixed effect, treatment assignment, possible simulation
village_fe <- runif(1000, 0,1)
village_ta <- runif(1000, 0,1)
village_rv <- runif(1000, 0,1)

data <- t(cbind(data, village_fe, village_ta, village_rv))

# 2 time periods per village
data <- matrix(expand_r(data, 2), ncol=4, byrow=T)
time <- c(replicate(1000,1), replicate(1000,2))
data <- data.frame(cbind(data, time))

colnames(data) <-  c("vid", "vfe", "vta", "vrv", "time")

# Assign if/when treatment is administered
data$nt     <- as.numeric(data$vta<cutoff1)
data$tpast  <- as.numeric(data$vta>=cutoff1 & data$vta<cutoff2)
data$tnow   <- as.numeric(data$vta>=cutoff2)

# Assign treatment variables
data$treated  <- as.numeric((data$tpast==1 & data$time==1) | (data$tnow==1 & data$time==2))
# Observed treatment: Two alternatives
data$tobs1    <- as.numeric((data$tnow==1 & data$time==2))
data$tobs2    <- as.numeric((data$tnow==1 & data$time==2) | (data$tpast==1 & data$time==1) | (data$nt==1 & data$time==1))



## Data-generating process
# Treatment effect
data$teffect <- rnorm(2000, 2, 1^2)
# Error term
data$error 	<- rnorm(2000, 0, 1^2)

# Generate outcome variable
# Note that fixed effect is random + treatment assignment which depends on Fixed Effect:
# Otherwise, pooled OLS will do fine.
data$y <- data$vfe + data$vta + data$time + data$treated*data$teffect + data$error

# Declare data panel data
data <- pdata.frame(data, index = c("vid","time"), drop.index = FALSE)

## For alternative to re-weighing:
data$y2 <- data$y
mean_nt_pre   <- mean(data$y[data$nt==1 & data$time==1])  
mean_nt_post  <- mean(data$y[data$nt==1 & data$time==2])  
mean_tp_pre   <- mean(data$y[data$tpast==1 & data$time==1])  
mean_tp_post  <- mean(data$y[data$tpast==1 & data$time==2]) 
mean_tn_pre   <- mean(data$y[data$tnow==1 & data$time==1])  
mean_tn_post  <- mean(data$y[data$tnow==1 & data$time==2]) 

data$y2[data$time==1] <- data$y[data$time==1] - mean_nt_pre
data$y2[data$time==2] <- data$y[data$time==2] - mean_nt_post

data$temp1 <- lead(data$y2, k=1)
data$temp2 <- lag(data$y2, k=1)
data$y2[data$time==1 & data$tpast==1] <- data$temp1[data$time==1 & data$tpast==1]
data$y2[data$time==2 & data$tpast==1] <- data$temp2[data$time==2 & data$tpast==1]

data$treated2 <- 0
data$treated2[data$time==2 & data$tpast==1] <- 1
data$treated2[data$time==2 & data$tnow==1] <- 1


## Analysis

# Declare model
ols <- plm(y ~ treated, data=data,
        model="pooling")$coefficients[2]

did_true <- plm(y ~ treated, data=data,
    model="within", effect="twoways")$coefficients[1]

did1 <- plm(y ~ tobs1, data=data,
                model="within", effect="twoways")
did_obs1 <- did1$coefficients[1]

did2 <- plm(y ~ tobs2, data=data,
                model="within", effect="twoways")
did_obs2 <- did2$coefficients[1]

did_se1 <- sqrt(did1$vcov)
did_se2 <- sqrt(did2$vcov)

did_rw <- plm(y2 ~ treated2, data=data,
              model="within", effect="twoways")$coefficients[1]

results <- rbind(ols, did_true, did_obs1, did_obs2, did_se1, did_se2, did_rw)
}


## First simulation
simulationa <- lapply(1:n, simulate_did)

simulationa <- data.frame(matrix(unlist(simulationa), ncol=7, byrow=TRUE))
colnames(simulationa) <-  c("ols", "did_true", "did_t0", "did_t1", "se_t0", "se_t1", "did_rw")

simulationa$weighted <- cutoff1*simulation$did_t0 + cutoff2*simulationa$did_t1
simulationa$weighted <- 1/(1+(cutoff2-cutoff1)/cutoff2)*(cutoff1/cutoff2*simulationa$did_t0 + 2*(cutoff2-cutoff1)/cutoff2*simulationa$did_t1)



## COLORS
## Dark Green: '#0a6264'
## Orange Red: '#db4324'
## Blue: "#004b73"
## Yellow Before: "#d3ce8b"
## Yellow Now: "#d3ce8b"


# Make plots 1
{
p1a <-  ggplot(data=simulationa) + 
  geom_histogram(aes(did_true,  fill="#db4324",  color="#db4324"), alpha = 0.9,  binwidth=0.025) +
  geom_histogram(aes(did_t0, fill = "#004b73",color="#004b73"),  alpha = 0.9,  binwidth=0.025) +
  geom_histogram(aes(did_t1,  fill = "#d3ce8b",color="#d3ce8b"), alpha = 0.9,  binwidth=0.025) +
  geom_vline(xintercept = 2, linetype="solid", size=2.0) +
  labs(x="Estimated Effect (True=2)", y="") + theme_minimal() +
  theme(legend.position="bottom") +
  ggtitle("Cutoffs = 0.2/0.8") +
  scale_fill_manual(name="",
                    values=c("#db4324"="#db4324","#004b73"="#004b73","#d3ce8b"="#d3ce8b"),
                    labels = c("All treated", "Full information", "None treated")) +
  scale_color_manual(name="",
                     values=c("#db4324"="#db4324","#004b73"="#004b73","#d3ce8b"="#d3ce8b"),
                     labels = c("All treated", "Full information", "None treated")) +
  theme(text=element_text(family="Palatino Linotype")) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt"))) +
  theme(panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))










p2a <-  ggplot(data=simulationa) + 
  geom_histogram(aes(did_true,  fill="#db4324",  color="#db4324"), alpha = 0.9,  binwidth=0.025) +
  geom_histogram(aes(weighted, fill='#0a6264', col='#0a6264'), alpha = 0.6, binwidth=0.025) +
  labs(x="Estimated Effect (True=2)", y="") + theme_minimal() +
  theme(legend.position="bottom") +
  ggtitle("Cutoffs = 0.2/0.8") +
  xlim(1.7, 2.3) +
  geom_vline(xintercept = 2, linetype="solid", size=2.0) +
  scale_fill_manual(name="",
                    values=c("#db4324"="#db4324",'#0a6264'='#0a6264'),
                    labels = c("Full information", "Weighted Estimate")) +
  scale_color_manual(name="",
                     values=c("#db4324"="#db4324",'#0a6264'='#0a6264'),
                     labels = c("Full information", "Weighted Estimate")) +
  theme(text=element_text(family="Palatino Linotype")) +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt"))) +
  theme(panel.grid.minor = element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5))
print(p2a)



tikz(file="dist_before_0208.tex", standAlone=F,
     width = 5, height = 4)
print(p1a)
dev.off()

tikz(file="dist_after_0208.tex", standAlone=F,
     width = 5, height = 4)
print(p2a)
dev.off()
}
 
 
 
 ###########
 
 cutoff1 <- 1/3
 cutoff2 <- 2/3
 
 ##
 simulationb <- lapply(1:n, simulate_did)
 
 simulationb <- data.frame(matrix(unlist(simulationb), ncol=7, byrow=TRUE))
 colnames(simulationb) <-  c("ols", "did_true", "did_t0", "did_t1", "se_t0", "se_t1", "did_rw")
 
 simulationb$weighted <- cutoff1*simulationb$did_t0 + cutoff2*simulationb$did_t1
 simulationb$weighted <- 1/(1+(cutoff2-cutoff1)/cutoff2)*(cutoff1/cutoff2*simulationb$did_t0 + 2*(cutoff2-cutoff1)/cutoff2*simulationb$did_t1)
 
 ##Make plots 2
 {
   p1b <-  ggplot(data=simulationb) + 
   geom_histogram(aes(did_true,  fill="#db4324",  color="#db4324"), alpha = 0.9,  binwidth=0.025) +
   geom_histogram(aes(did_t0, fill = "#004b73",color="#004b73"),  alpha = 0.9,  binwidth=0.025) +
   geom_histogram(aes(did_t1,  fill = "#d3ce8b",color="#d3ce8b"), alpha = 0.9,  binwidth=0.025) +
   geom_vline(xintercept = 2, linetype="solid", size=2.0) +
   labs(x="Estimated Effect (True=2)", y="") + theme_minimal() +
   theme(legend.position="bottom") +
   ggtitle("Cutoffs = 1/3, 2/3") +
   scale_fill_manual(name="",
                     values=c("#db4324"="#db4324","#004b73"="#004b73","#d3ce8b"="#d3ce8b"),
                     labels = c("All treated", "Full information", "None treated")) +
   scale_color_manual(name="",
                      values=c("#db4324"="#db4324","#004b73"="#004b73","#d3ce8b"="#d3ce8b"),
                      labels = c("All treated", "Full information", "None treated")) +
   theme(text=element_text(family="Palatino Linotype")) +
   theme(legend.text = element_text(margin = margin(r = 30, unit = "pt"))) +
   theme(panel.grid.minor = element_blank()) +
   theme(plot.title = element_text(hjust = 0.5))
 
 
 p2b <-  ggplot(data=simulationb) + 
   geom_histogram(aes(did_true,  fill="#db4324",  color="#db4324"), alpha = 0.9,  binwidth=0.025) +
   geom_histogram(aes(weighted, fill='#0a6264', col='#0a6264'), alpha = 0.6, binwidth=0.025) +
   labs(x="Estimated Effect (True=2)", y="") + theme_minimal() +
   theme(legend.position="bottom") +
   ggtitle("Cutoffs = 1/3, 2/3") +
   xlim(1.7, 2.3) +
   geom_vline(xintercept = 2, linetype="solid", size=2.0) +
   scale_fill_manual(name="",
                     values=c("#db4324"="#db4324",'#0a6264'='#0a6264'),
                     labels = c("Full information", "Weighted Estimate")) +
   scale_color_manual(name="",
                      values=c("#db4324"="#db4324",'#0a6264'='#0a6264'),
                      labels = c("Full information", "Weighted Estimate")) +
   theme(text=element_text(family="Palatino Linotype")) +
   theme(legend.text = element_text(margin = margin(r = 30, unit = "pt"))) +
   theme(panel.grid.minor = element_blank()) + 
   theme(plot.title = element_text(hjust = 0.5))
 
 
 tikz(file="dist_before_1323.tex", standAlone=F,
      width = 5, height = 4)
 print(p1b)
 dev.off()
 
 tikz(file="dist_after_1323.tex", standAlone=F,
      width = 5, height = 4)
 print(p2b)
 dev.off()
 }
 
 
 
 
 
 
 
 #############
 

 cutoff1 <- 1/3
 cutoff2 <- 1/2
 ##
 simulationc <- lapply(1:n, simulate_did)
 
 simulationc <- data.frame(matrix(unlist(simulationc), ncol=7, byrow=TRUE))
 colnames(simulationc) <-  c("ols", "did_true", "did_t0", "did_t1", "se_t0", "se_t1", "did_rw")
 
 simulationc$weighted <- cutoff1*simulationc$did_t0 + cutoff2*simulationc$did_t1
 simulationc$weighted <- 1/(1+(cutoff2-cutoff1)/cutoff2)*(cutoff1/cutoff2*simulationc$did_t0 + 2*(cutoff2-cutoff1)/cutoff2*simulationc$did_t1)
 
 ## Make plots 3
 
 {
 p1c <-  ggplot(data=simulationc) + 
   geom_histogram(aes(did_true,  fill="#db4324",  color="#db4324"), alpha = 0.9,  binwidth=0.025) +
   geom_histogram(aes(did_t0, fill = "#004b73",color="#004b73"),  alpha = 0.9,  binwidth=0.025) +
   geom_histogram(aes(did_t1,  fill = "#d3ce8b",color="#d3ce8b"), alpha = 0.9,  binwidth=0.025) +
   geom_vline(xintercept = 2, linetype="solid", size=2.0) +
   labs(x="Estimated Effect (True=2)", y="") + theme_minimal() +
   theme(legend.position="bottom") +
   ggtitle("Cutoffs = 1/3, 1/2") +
   scale_fill_manual(name="",
                     values=c("#db4324"="#db4324","#004b73"="#004b73","#d3ce8b"="#d3ce8b"),
                     labels = c("All treated", "Full information", "None treated")) +
   scale_color_manual(name="",
                      values=c("#db4324"="#db4324","#004b73"="#004b73","#d3ce8b"="#d3ce8b"),
                      labels = c("All treated", "Full information", "None treated")) +
   theme(text=element_text(family="Palatino Linotype")) +
   theme(legend.text = element_text(margin = margin(r = 30, unit = "pt"))) +
   theme(panel.grid.minor = element_blank()) +
   theme(plot.title = element_text(hjust = 0.5))
 
 
 p2c <-  ggplot(data=simulationc) + 
   geom_histogram(aes(did_true,  fill="#db4324",  color="#db4324"), alpha = 0.9,  binwidth=0.025) +
   geom_histogram(aes(weighted, fill='#0a6264', col='#0a6264'), alpha = 0.6, binwidth=0.025) +
   labs(x="Estimated Effect (True=2)", y="") + theme_minimal() +
   xlim(1.7, 2.3) +
   theme(legend.position="bottom") +
   ggtitle("Cutoffs = 1/3, 1/2") +
   geom_vline(xintercept = 2, linetype="solid", size=2.0) +
   scale_fill_manual(name="",
                     values=c("#db4324"="#db4324",'#0a6264'='#0a6264'),
                     labels = c("Full information", "Weighted Estimate")) +
   scale_color_manual(name="",
                      values=c("#db4324"="#db4324",'#0a6264'='#0a6264'),
                      labels = c("Full information", "Weighted Estimate")) +
   theme(text=element_text(family="Palatino Linotype")) +
   theme(legend.text = element_text(margin = margin(r = 30, unit = "pt"))) +
   theme(panel.grid.minor = element_blank()) + 
   theme(plot.title = element_text(hjust = 0.5))
 print(p2c)
 
 tikz(file="dist_before_1305.tex", standAlone=F,
      width = 5, height = 4)
 print(p1c)
 dev.off()
 
 tikz(file="dist_after_1305.tex", standAlone=F,
      width = 5, height = 4)
 print(p2c)
 dev.off()
 }
 
 
 
