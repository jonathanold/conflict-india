remotes::install_github("kolesarm/RDHonest")
library(RDHonest)
RDHonestBME(log(cghs$earnings)~yearat14, data=cghs, h=3,
            order=1, cutoff=1947)


rdddata = complete_data@data

a <- RDHonestBME(
  formula = sc_reserved ~ rank_rdd_2,
  data = rdddata,
  cutoff = 0,
  na.action=na.omit,
  h = Inf,
  alpha = 0.05,
  order = 1
)
a