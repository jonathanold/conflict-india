

data_conley$const <- 1
olsConley <- function(data, y, X, lat, lon, cutoff) {

  data=data_conley
  y="acled_totalevents"
  X=c("const", "sc_reserved", "prop_sc", "prop_schools")
  lat="y_midpoint"
  lon="x_midpoint"
  cutoff="250"
  
  # usual setup 
  n <- nrow(data)
  k <- length(X)
  ydata <- tbldfGrabber(data, y)
  xdata <- tbldfGrabber(data, X)
  betahat <- solve(t(xdata) %*% xdata) %*% t(xdata) %*% ydata
  e <- ydata - xdata %*% betahat
  
  # grab latitude & longitude
  latdata <- tbldfGrabber(data, lat)
  londata <- tbldfGrabber(data, lon)
  
  

  # loop over all of the spatial units (aka observations)
  meatWeight <- lapply(1:n, function(i) {
    #  turn longitude & latitude into KMs. 1 deg lat = 111 km, 1 deg lon = 111 km* cos(lat)
    lonscale <- cos(latdata[i]*pi / 180) * 111
    latscale <- 111
    
    # distance --> use pythagorean theorem! who knew that was useful?
    dist <- as.numeric(sqrt((latscale*(latdata[i] - latdata))^2 
                            + (lonscale*(londata[i] - londata))^2))
    
    # set a window var = 1 iff observation j is within cutoff dist of obs i 
    window <- as.numeric(dist <= cutoff)
    # this next part is where the magic happens. this thing makes:
    #    sum_j(X_iX_j'e_ie_j K(d_{ij})), and we make n of them - one for each i.
    
    
    # double transpose here is because R is bad at dealing with 1 x something stuff.
    #   we want x_i'; this is an easy way to get it. Now for some dimensions
    #   (we want k x k at the end):
    
    XeeXh <-  ((t(t(xdata[i, ]))  %*% matrix(1, 1, n) * e[i,]) * (matrix(1, k, 1) %*% (t(e) * t(window)))) %*% xdata 
    return(XeeXh)
    
  })
  
  # phew! Now let's make our sandwich. First, the meat = sum_i what we just made
  meat <- (Reduce("+", meatWeight)) / n
  # and the usual bread  
  bread <- solve(t(xdata) %*% xdata)
  # mmmm, delicious sandwich
  sandwich  <- n* (t(bread) %*% meat %*% bread)
  sandwich <- (sandwich + t(sandwich)) / 2
  # se as per usual
  se <- sqrt(diag(sandwich))
  output <- list(betahat, se)
  names(output) <- c("betahat", "conleySE")
  return(output)
}
