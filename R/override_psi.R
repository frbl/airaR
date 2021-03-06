globalVariables(c("reverse_order"))

myPsi.varest <- function(x, nstep=10, ...){
    if(!(class(x)=="varest")){
      stop("\nPlease provide an object of class 'varest', generated by 'VAR()'.\n")
    }
    nstep <- abs(as.integer(nstep))

    # First calculate the normal irf using the phi
    Phi <- Phi(x, nstep = nstep)

    # Dim of phi is [#variables, #variables, horizon]
    Psi <- array(0, dim=dim(Phi))

    # Params contains all exo parameters
    params <- ncol(x$datamat[, -c(1:x$K)])

    # This is more or less equal to the covariance matrix
    # cov(resid(x)) - (crossprod(resid(x)) / (x$obs-1)) == 0
    sigma.u <- crossprod(resid(x)) / (x$obs - params)
    #sigma.u <- (crossprod(resid(x)) / (x$obs-14))

    # perform the cholesky decomposition
    P <- t(chol(sigma.u))

    if(reverse_order) {
      P <- t(P)
    }

    # Dim3 is the horizon
    dim3 <- dim(Phi)[3]
    for(i in 1:dim3){
      Psi[, , i] <- Phi[, , i] %*% P
    }
    return(Psi)
  }

override_function <- function(origfuncname,packagename,newfunc) {
  failed <- FALSE
  tryCatch(unlockBinding(origfuncname, as.environment(paste("package:",packagename,sep=''))),
           error=function(e) failed <<- TRUE)
  if (!failed) assign(origfuncname, newfunc, paste("package:",packagename,sep=''))
  unlockBinding(origfuncname, getNamespace(packagename))
  assign(origfuncname, newfunc, getNamespace(packagename))
}

