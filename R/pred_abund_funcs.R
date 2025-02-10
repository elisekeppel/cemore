

# summod <- function(mod, newdata, off.set = newdata$off.set, abund = F, pred_by = NULL, forplot = F){#
#   # mod.name <- deparse(substitute(mod))
#   summ     <- summary(mod, digits = 3)
#   if(summ$family$family %like% "Negative Binomial") fam <- "nb"
#   if(summ$family$family %like% "Tweedie") fam <- "tw"
#
#   # Calculate overdispersion factor
#   E1   <- resid(mod, type = "pearson")
#   N    <- nrow(mod$data)      # sample size
#   np   <- summ$np             # number of parameters
#
#   if(summ$m==1) k <- as.character(np)
#   if(!summ$m==1) k <- paste0(summ$s.table[,2] + 1, sep="", collapse = ", ")
#   #
#   df <- data.frame(
#     count   = sum(mod$data$count),
#     # model   = mod.name,
#     dist    = fam,
#     k       = k,
#     smooth  = paste(rownames(summ$s.table), collapse=", "),
#     disp    = round(sum(E1^2) / (N - np), digits=3),
#     dev.exp = round(summ$dev.expl*100, digits =1),
#     AIC     = round(AIC(mod), digits=2))
#   if(!is.null(summ$pTerms.pv)){
#     df$linear <- paste(row.names(summ$pTerms.pv), collapse=", ")
#     df$p.linear <- paste(signif(summ$pTerms.pv, digits = 3), collapse=", ")
#   }
#
#   if(abund){
#     # if(is.null(pred_by)){
#     #   ab <- PRED_(mod, newdata = newdata, off.set = newdata$off.set)
#     #   # ab <- round(sum(predict(mod, newdata = newdata, off.set=newdata$off.set), na.rm = T), digits=0)
#     #   df$ab.est <- ab[,2]
#     # }else{
#       ab <- pred_ab_spt(mod, newdata = newdata, pred_by = pred_by)
#       df$ab.est <- ab[,2]
#       df$model <- ab[,1]
#   }
#
#   if(forplot){
#     df <- t(df) %>% as.table()
#     dimnames(df)[[2]] <- ""
#   }
#   df
# }

# just updating the above version to paper format
summod <- function(mod, newdata, off.set = newdata$off.set, abund = F, pred_by = NULL, forplot = F){#
  # mod.name <- deparse(substitute(mod))
  summ     <- summary(mod, digits = 3)
  # if(summ$family$family %like% "Negative Binomial") fam <- "nb"
  # if(summ$family$family %like% "Tweedie") fam <- "tw"

  fam <- summ$family$family
  # Calculate overdispersion factor
  E1   <- resid(mod, type = "pearson")
  N    <- nrow(mod$data)      # sample size
  np   <- summ$np             # number of parameters

  if(summ$m==1) k <- as.character(np)
  if(!summ$m==1) k <- paste0(summ$s.table[,2] + 1, sep="", collapse = ", ")

  smooths <- rownames(summ$s.table)
  # remove their ending bracket, add a comma
  smooths <- sub(")", ",", smooths)
  # paste in their edfs, rounded
  smooths <- paste(smooths, signif(summ$s.table[,1], 3))
  # close the bracket again
  smooths <- paste(smooths, ")", sep="")
  # paste them all together in a list into dat
  smooths <- paste(smooths, collapse=",\n")

  if(!is.null(summ$pTerms.pv)){
    Linear.Terms <- paste(row.names(summ$pTerms.pv), collapse=", ")
    # df$p.linear <- paste(signif(summ$pTerms.pv, digits = 3), collapse=", ")
  }

  df <- data.frame(
    count   = sum(mod$data$count),
    sgts = nrow(mod$data[which(mod$data$count>0),]),
    # model   = mod.name,
    Family    = fam,
    # k       = k,
    # smooth  = paste(rownames(summ$s.table), collapse=", "),
    Smooths = smooths,
    Linear.Terms,
    # disp    = round(sum(E1^2) / (N - np), digits=3),
    Dev.Exp. = round(summ$dev.expl*100, digits =1),
    AIC     = round(AIC(mod), digits=2))


  if(abund){
    # if(is.null(pred_by)){
    #   ab <- PRED_(mod, newdata = newdata, off.set = newdata$off.set)
    #   # ab <- round(sum(predict(mod, newdata = newdata, off.set=newdata$off.set), na.rm = T), digits=0)
    #   df$ab.est <- ab[,2]
    # }else{
    ab <- pred_ab_spt(mod, newdata = newdata, pred_by = pred_by)
    df$Est.Ab <- ab[,2]
    # df$model <- ab[,1]
  }

  if(forplot){
    df <- t(df) %>% as.table()
    dimnames(df)[[2]] <- ""
  }
  df
}


# summarize_dsm2 <- function(model){
#
#   summ <- summary(model)
#
#
#   dat <- data.frame(response = model$family$family,
#                     AIC      = AIC(model))
#
#   # take the smooth names
#   smooths <- rownames(summ$s.table)
#   # remove their ending bracket, add a comma
#   smooths <- sub(")", ",", smooths)
#   # paste in their edfs, rounded
#   smooths <- paste(smooths, signif(summ$s.table[,1], 3))
#   # close the bracket again
#   smooths <- paste(smooths, ")", sep="")
#   # paste them all together in a list into dat
#   dat$terms <- paste(smooths, collapse=", ")
#
#   return(dat)
# }

modsums <- function(mod_list, newdata, off.set = newdata$off.set, abund = T, pred_by = F) {
  print(paste("not working - use ab_sum_pred_by() instead"))
  # if(any(class(mod_list) == "list")){
  #   names <- names(mod_list)
  # }else{
  #   names <- deparse(substitute(mod_list))
  #   mod_list <- lst(mod_list)
  # }
  #
  # res <- purrr::map_df(mod_list, summod, newdata = newdata, off.set = newdata$off.set, abund = abund, pred_by = pred_by)#, abund
  # n <- names(res)
  # res$model <- names
  # res %<>% select(model, n)
  # if(unique(res$model %like% "mod")) res$model <- NULL
  # res %<>% arrange(dist, AIC)
  #
  # # if(abund){
  # #   ab <- purrr::map_df(mod_list, pred.ab, newdata = newdata, off.set = newdata$off.set)
  # #   ab$model <- names
  # #   if(unique(ab$model %like% "mod")) ab$model <- NULL
  # #   res <- inner_join(res, ab)
  # # }
  # res
  # # }
}

#############################################################################
pred.ab <- function(mod, newdata, off.set = newdata$off.set) {
  # print("Note: input is required to be a named list created with lst()" if you want names output when used with purrr::map)
  # newdata$monthYear <- newdata$monthYear %>% droplevels()

  model <- deparse(substitute(mod))
  pred <-  round(sum(predict(mod, newdata = newdata, off.set=newdata$off.set), na.rm = T), digits=0)
  data.frame(model, pred)
}


ab_sum_pred_by <- function(mod_list, abund = T, newdata = NULL, pred_by = NULL){#}, res="sum"){
  if(any(class(mod_list) == "list")){
    names <- names(mod_list)
  }else{
    names <- deparse(substitute(mod_list)) # for individual calls
    mod_list <- lst(mod_list)
  }
  # res <- purrr::map_df(mod_list, pred_ab_spt, newdata = newdata, pred_by = pred_by)#, res="sum")
  res <- purrr::map_df(mod_list, summod, abund = T, newdata = newdata, pred_by = pred_by)#, res="sum")
  # res$model <- names
  # if(unique(res$model %like% "mod")) res$model <- NULL
  res %<>% arrange(Family, AIC)

  res
}

# APPARENTLY THIS WAS IN THE DSM COURSE DAY 5 RMD
# # take the smooth names
# smooths <- rownames(summ$s.table)
# # remove their ending bracket, add a comma
# smooths <- sub(")", ",", smooths)
# # paste in their edfs, rounded
# smooths <- paste(smooths, signif(summ$s.table[,1], 3))
# # close the bracket again
# smooths <- paste(smooths, ")", sep="")
# # paste them all together in a list into dat
# dat$terms <- paste(smooths, collapse=", ")


# ab_sum <- function(mod_list, newdata){
#   if(any(class(mod_list) == "list")){
#     names <- names(mod_list)
#   }else{
#     names <- deparse(substitute(mod_list)) # for individual calls
#     mod_list <- lst(mod_list)
#   }
#   res <- purrr::map_df(mod_list, pred.ab, newdata = newdata)
#   res$model <- names
#   if(unique(res$model %like% "mod")) res$model <- NULL
#   res
# }


# if(sum(grep("fYear", res$smooth) ,
#        grep("sst", res$smooth) ,
#        grep("sqr.chl", res$smooth) ,
#        grep("fYear", res$factor) ,
#        grep("sst", res$factor) ,
#        grep("sqr.", res$factor)) >= 1){
#   newdata <- predgrid.my5
#   print("note: using predgrid.my5")
# }else{
#   newdata <- predgrid
#   print("note: using predgrid")
# }


# if(is.null(newdata)) {
#   newdata <- predgrid
#   print("note: using predgrid.my5")
# }
# summ <- summary(mod)
# if(sum(grep("fYear", dimnames(summ$s.table)) ,
#        grep("sst", dimnames(summ$s.table)) ,
#        grep("sqr.chl", dimnames(summ$s.table)) ,
#        grep("fYear", dimnames(summ$pTerms.pv)) ,
#        grep("sst", dimnames(summ$pTerms.pv)) ,
#        grep("sqr.", dimnames(summ$pTerms.pv))) >= 1){
#   newdata <- predgrid.my5
#   print("note: using predgrid.my5")
# }else{
#   newdata <- predgrid
#   print("note: using predgrid")
# }

