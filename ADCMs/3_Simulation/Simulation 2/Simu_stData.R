# g0 <- function(t){
#   t <- 1- t
#   z <- 0.2 * t^11 * (10 * (1 - t))^6 + 10 * (10 * t)^3 * (1 - t)^10
#   return((z))
# }
# g1 <- function(t){
#   z <- 0.2 * t^11 * (10 * (1 - t))^6 + 10 * (10 * t)^3 * (1 - t)^10
#   return(z)
# }
Simu_stData <- function(para, site = NULL, W_ts = NULL){
  time <- seq(0, 1,, para$Nt)
  if(is.null(site)){
    # time <- 1:para$Nt
    load("./data/CMAQ_Coordinate.RData")
    loc <- cbind(1:nrow(CMAQ_Coordinate), CMAQ_Coordinate[, c("LON", "LAT")])
    
    colnames(loc) <- c("ID", "LON", "LAT")
    
    library(sp)
    site <- spCoords.transform(as.data.frame(loc), method  = 2)
    # plot(site[, 2:3])
    site <- as.data.frame(site)
    
    
    para$n <- nrow(site)
    
    RFoptions(spConform=F, modus_operandi="sloppy")
    model <- RMnsst(
      # phi = RMexp(scale = para$Phis,
      #             var = para$sigma.sq.s),
      phi = RMmatern(nu = para$nu,
                     var = 1e0,#para$sigma.sq.s,
                     scale = para$scale_s
      ), #C(r) = (1 + r^α)^(-β/α)
      psi = RMgencauchy(alpha = para$a, #l (0,2].
                        beta = para$sep, #> 0
                        scale = para$scale_t, #para$sigma.sq.t,
                        var = 1e0
      ),
      # C(r) = (1 + 8 s r + 25 s^2 r^2 + 32 s^3 r^3)(1-s r)^8 if 0 <= r <= 1/s 
      # psi = RMgneiting(orig = F,  #l (0,2].
      #                var = 1e0, #para$sigma.sq.t,
      #                scale = para$scale_t),   
      # psi = RMgenfbm(alpha =  para$sep, #l (0,2].
      #                var = 1e0, #para$sigma.sq.t,
      #                scale = para$scale_t,
      #                beta = 1e0),   #(0,2].
      delta = para$delta,#para$delta, # >= 2
      var = para$sill
    ) #+ RMnugget(var = para$tau.sq)
    
    W_ts <- RFsimulate(model, x = site$LON_X, y = site$LAT_Y, T = time) %>%
      matrix(nrow = nrow(site), ncol = para$Nt)%>% t()
    
    W_ts <- W_ts - mean(W_ts)
  }else{
    id <- sample(1:nrow(site), para$n, replace = F)
    site$Simu <- ifelse(site$ID %in%id, "Train", "Test")
    # W_ts <- W_ts[, id]
    
    cat(unique(site$Simu), "\n------\n")
  }
  para$n <- nrow(site)
  
  
  #######################################################################
  #######################################################################
  
  # simulate data
  Y_ts <- matrix(NA, nrow = para$Nt, ncol = para$n)
  colnames(Y_ts) <- 1:para$n
  
  X_ts <- array(0, dim = c(3, para$Nt, para$n),
                dimnames = list(c("intercept", "X1", "X2"), 
                                c(1:para$Nt),
                                1:para$n))
  
  # library(mvnfast)
  setDF(site)
  
  D <- fields::rdist(site[, 4:5], site[, 4:5])
  # D$range
  sigma.sq <- matrix(rnorm(para$n*para$Nt, 0, sqrt(para$nugget)),
                     nrow = para$Nt)
  x.phi <- 0.1*max(D)
  L <- Matrix::chol(fields::Matern(d = D, range = x.phi, smoothness = 0.5, phi = 1))
  
  for(t in 1:para$Nt)
  {
    X_ts[1, t, ] <- 1#abs(site[,2] - 0.5)^2
    X_ts[2, t, ] <- rnorm(para$n, 0, 1)
    X_ts[3, t, ] <- Matrix::crossprod(L, rep(rnorm(para$n)))
    mu =  as.vector(t(X_ts[1:3, t, ]) %*% para$beta)
    Y_ts[t, ] <- mu +  W_ts[t, ] + sigma.sq[t, ] 
  }
  re <- list(Site = site, Y_ts = Y_ts, 
             X_ts = X_ts, W_ts = W_ts,                     
             Site = site,
             siteid = "ID")
  return(re)
}
