{
  
  emodel <-  acled_totalevents ~ sc_reserved + sc_prop_01_norm + factor(DT_NAME) +  
    pc01_pca_tot_p_norm + 
    pc01_vd_p_sch_norm + 
    pc01_vd_tar_road_norm 
  fmodel <-  fat ~ sc_reserved + sc_prop_01_norm +  factor(DT_NAME) +  
    pc01_pca_tot_p_norm +
    pc01_vd_p_sch_norm + 
    pc01_vd_tar_road_norm 
  logmodel <- log(acled_totalevents+1) ~ sc_reserved + sc_prop_01_norm +  factor(DT_NAME) + 
    pc01_pca_tot_p_norm + 
    pc01_vd_p_sch_norm + 
    pc01_vd_tar_road_norm 
  logmodelf <- log(fat+1) ~ sc_reserved + sc_prop_01_norm +  factor(DT_NAME) + 
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
  
  
  summary(nb1 <- glm.nb(formula=emodel ,
                        data=data_subsetted))
  summary(nb2 <- glm.nb(formula=fmodel ,
                        data=data_subsetted))
  summary(ps1 <- glm(formula=emodel ,
                     data=data_subsetted, family="poisson"))
  summary(ps2 <- glm(formula=fmodel ,
                     data=data_subsetted, family="poisson"))
  summary(ols1 <- lm(formula = logmodel ,
                     data=data_subsetted))
  summary(ols2 <- lm(formula = logmodelf ,
                     data=data_subsetted))
  
  summary(nb11 <- glm.nb(formula=emodel1,
                         data=data_subsetted))
  summary(nb21 <- glm.nb(formula=fmodel1 ,
                         data=data_subsetted))
  summary(ps11 <- glm(formula=emodel1 ,
                      data=data_subsetted, family="poisson"))
  summary(ps21 <- glm(formula=fmodel1 ,
                      data=data_subsetted, family="poisson"))
  summary(ols11 <- lm(formula = logmodel1 ,
                      data=data_subsetted))
  summary(ols21 <- lm(formula = logmodelf1 ,
                      data=data_subsetted))
  
  mean(data_subsetted@data$acled_totalevents)
  var(data_subsetted@data$acled_totalevents)
  
  mean(data_subsetted@data$fat)
  var(data_subsetted@data$fat)
  
  
  stargazer(ols11, ols1, ps11, ps1, nb11, nb1,
            title = "Results: Constituency assemblies",
            keep = c('sc_reserved', 'sc_prop_01_norm','Constant'),
            covariate.labels = c("\\textbf{Seat reserved for SC}",
                                 "Percent SC population", "Constant"),
            no.space=TRUE,
            column.labels   = c("OLS: log(Events+1)", "Poisson", "Negative binomial"),
            column.separate = c(2, 2, 2),
            model.names = F,
            dep.var.caption = "",
            dep.var.labels.include = F,
            omit.stat=c("ser", "theta", "f" ),
            notes = c("Results from regressions of conflict events/fatalities for constituency assemblies in the sample",
                      "as described in the main text.",
                      "OLS is for ordinary least squares, NB for negative binomial regression models.",
                      "Signficance levels: $^*10\\%, ^{**}5\\%, ^{***}1\\%$.\\label{tab:assembly}"),
            notes.align = "l",
            notes.append=F,
            notes.label = "\\textbf{Note:}",
            out="./Output/tables/assemblyreg_caste.tex")
  
  
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
                      "Signficance levels: $^*10\\%, ^{**}5\\%, ^{***}1\\%$.\\label{tab:assemblyps}"),
            notes.align = "l",
            notes.append=F,
            notes.label = "\\textbf{Note:}",
            out="./Output/tables/assemblyreg_ps.tex")
}