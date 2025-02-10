#
# summod <- function(mod, newdata, off.set = newdata$off.set, abund = F, forplot = F, split_by){#
#   mod.name <- deparse(substitute(mod))
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
#     model   = mod.name,
#     dist    = fam,
#     k       = k,
#     smooth  = paste(rownames(summ$s.table), collapse=", "),
#     disp    = round(sum(E1^2) / (N - np), digits=3),
#     dev.exp = round(summ$dev.expl*100, digits =1),
#     AIC     = round(AIC(mod), digits=2))
#   if(!is.null(summ$pTerms.pv)){
#     df$factor <- paste(row.names(summ$pTerms.pv), collapse=", ")
#     df$p.fact <- paste(signif(summ$pTerms.pv, digits = 3), collapse=", ")
#   }
#   if(abund){
#     if(is.null(split_by)){
#       ab <- pred.ab(mod, newdata = newdata, off.set = newdata$off.set)
#       # ab <- round(sum(predict(mod, newdata = newdata, off.set=newdata$off.set), na.rm = T), digits=0)
#       df$ab.est <- ab[,2]
#     }else{
#       ab <- pred_ab_by_my(mod, newdata = newdata, split_by = split_by)
#       df$ab.est <- ab[,2]
#     }}
#   if(forplot){
#     df <- t(df) %>% as.table()
#     dimnames(df)[[2]] <- ""
#   }
#   df
# }
#
# modsums <- function(mod_list, newdata, off.set = newdata$off.set,
#                     abund = T, split_by = NULL) {
#   if(any(class(mod_list) == "list")){
#     names <- names(mod_list)
#   }else{
#     names <- deparse(substitute(mod_list))
#     mod_list <- lst(mod_list)
#   }
#
#   res <- purrr::map_df(mod_list, summod, newdata = newdata, off.set = newdata$off.set, abund = abund, split_by = split_by)#, abund
#   n <- names(res)
#   res$model <- names
#   res %<>% select(model, n)
#   if(unique(res$model %like% "mod")) res$model <- NULL
#   res %<>% arrange(dist, AIC)
#
#   # if(abund){
#   #   ab <- purrr::map_df(mod_list, pred.ab, newdata = newdata, off.set = newdata$off.set)
#   #   ab$model <- names
#   #   if(unique(ab$model %like% "mod")) ab$model <- NULL
#   #   res <- inner_join(res, ab)
#   res
# }
#
# #############################################################################
# pred.ab <- function(mod, newdata, off.set = newdata$off.set) {
#   # print("Note: input is required to be a named list created with lst()" if you want names output when used with purrr::map)
#   # newdata$monthYear <- newdata$monthYear %>% droplevels()
#
#   model <- deparse(substitute(mod))
#   pred <-  round(sum(predict(mod, newdata = newdata, off.set=newdata$off.set), na.rm = T), digits=0)
#   data.frame(model, pred)
# }
#
# ab_sum <- function(mod_list, newdata = NULL, split_by){
#   if(any(class(mod_list) == "list")){
#     names <- names(mod_list)
#   }else{
#     names <- deparse(substitute(mod_list)) # for individual calls
#     mod_list <- lst(mod_list)
#   }
#   if(split_by){
#     res <- purrr::map_df(mod_list, pred.ab, newdata = newdata)
#   }else{
#     res <- purrr::map_df(mod_list, pred_ab_by_my, newdata = newdata)
#   }
#   res$model <- names
#   if(unique(res$model %like% "mod")) res$model <- NULL
#   res
# }
#
# # APPARENTLY THIS WAS IN THE DSM COURSE DAY 5 RMD
# # # take the smooth names
# # smooths <- rownames(summ$s.table)
# # # remove their ending bracket, add a comma
# # smooths <- sub(")", ",", smooths)
# # # paste in their edfs, rounded
# # smooths <- paste(smooths, signif(summ$s.table[,1], 3))
# # # close the bracket again
# # smooths <- paste(smooths, ")", sep="")
# # # paste them all together in a list into dat
# # dat$terms <- paste(smooths, collapse=", ")
#
