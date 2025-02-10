dsm_var_prop.x <- function(dsm.obj, pred.data, off.set,
                         seglen.varname='Effort', type.pred="response") {
  # dsm.obj=dsm_nb_xy
  # pred.data=predgrid
  # off.set=predgrid$cell.area.km2
  # seglen.varname='Effort'
  # type.pred="response"
  # dsm.obj <- dsm.obj$dsm.object
  ## pre-chepredgrid## pre-checking...
  # die if we have a gamm
  if(any(class(dsm.obj)=="gamm")){
    stop("GAMMs are not supported.")
  }

  # break if we use the wrong response
  if(as.character(dsm.obj$formula)[2] != "count"){
    stop("Variance propagation can only be used with count as the response.")
  }

  # if there is no ddf object, then we should stop!
  # thanks to Adrian Schiavini for spotting this
  if(any(class(dsm.obj$ddf)=="fake_ddf")){
    stop("No detection function in this analysis, use dsm_var_gam")
  }

  ## end of checks

  ## data setup
  # if all the offsets are the same then we can just supply 1 and rep it
  if(length(off.set)==1){
    if(is.null(nrow(pred.data))){
      off.set <- rep(list(off.set), length(pred.data))
    }else{
      off.set <- rep(off.set, nrow(pred.data))
    }
  }

  # make sure if one of pred.data and off.set is not a list we break
  # if we didn't have a list, then put them in a list so everything works
  if(is.data.frame(pred.data) & is.vector(off.set)){
    pred.data <- list(pred.data)
    off.set <- list(off.set)
  }else if(is.list(off.set)){
    if(length(pred.data)!=length(off.set)){
      stop("pred.data and off.set don't have the same number of elements")
    }
  }

  # push the offsets into the data...
  for(i in seq_along(pred.data)){
    pred.data[[i]]$off.set <- off.set[[i]]
  }

  # mudge together all the prediction data
  all_preddata <- do.call("rbind", pred.data)

  ## end data setup


  # extract the link & invlink
  linkfn <- dsm.obj$family$linkfun
  linkinvfn <- dsm.obj$family$linkinv

  # storage
  vpred <- length(pred.data)
  preddo <- list()
  varp <- list()

  # to the varprop thing once to get the model
  varp <- dsm_varprop(dsm.obj, pred.data[[1]])
  refit <- varp$refit

  # add extra cols
  if(all(class(dsm.obj$ddf) == "list")){
    df_npars <- sum(unlist(lapply(dsm.obj$ddf,function(x) length(x$par))))
  }else{
    df_npars <- length(dsm.obj$ddf$par)
  }
  all_preddata[["XX"]] <- matrix(0, nrow(all_preddata), df_npars)

  # get a big Lp matrix now and just get rows below
  Lp_big <- predict(refit, newdata=all_preddata, type="lpmatrix")

  # start indices
  start <- 1
  end <- nrow(pred.data[[1]])

  # loop over the prediction grids
  for(ipg in seq_along(pred.data)){

    # get some data
    newdata <- pred.data[[ipg]]
    Lp <- Lp_big[start:end,,drop=FALSE]

    # predictions on the link scale
    pred <- Lp %*% coef(refit)
    pred <- newdata$off.set * linkinvfn(pred)

    # get variance-covariance
    vc <- refit$Vp

    # this is why we can only use log link
    dNdbeta <- t(pred)%*%Lp

    # make a sandwich
    var_p <- dNdbeta %*% vc %*% t(dNdbeta)

    # apply the link function to the offset
    # NB this is because refit is a gam not dsm object! If refit is dsm
    #    then this will get done in predict.dsm
    newdata$off.set <- linkfn(newdata$off.set)


    vpred[ipg] <- var_p
    preddo[[ipg]] <- sum(pred)

    # get next indices
    start <- end+1
    end <- start + nrow(pred.data[[ipg]])-1
  }


  result <- list(pred.var = vpred,
                 bootstrap = FALSE,
                 var.prop = TRUE,
                 pred.data = pred.data,
                 pred = preddo,
                 off.set = off.set,
                 model = varp$refit,
                 dsm.object = dsm.obj,
                 # model.check = dsm:::varprop_check(varp),
                 # deriv = firstD,
                 seglen.varname = seglen.varname,
                 type.pred=type.pred
  )

  class(result) <- "dsm.var"

  return(result)
}

