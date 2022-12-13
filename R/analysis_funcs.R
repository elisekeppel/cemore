get_df <- function(data, key = NULL, truncation){
  df <- ds(data = data, key = key, truncation = truncation)
  # s <- summary(df)
  # calculate effective strip width
  # print(s)
  # mu <- predict(df, esw=TRUE)$fitted[1]
  # f0 <- 1/mu
  #   + text(1,  y=0.75, paste("f(0) =", round(f0,2)))



  # cat(paste0("mu_", key, "_", truncation, " = ", mu))
}

plot_df <- function(species, df, details=T, showpoints=T,bins=NULL){
  # a <- df[[1]]$criterion %>% round(3) #AIC
  key <- df[[1]]$ds$aux$ddfobj$type
  key <- if(key=="hn") {"Half Normal"
  }else if(key=="hr") {"Hazard Rate"
  }else if(key=="un") {"Uniform"}
  # adj <- df[[1]]$ds$aux$ddfobj$adjustment$series
  # ord <- df[[1]]$ds$aux$ddfobj$adjustment$order %>% paste(collapse=",")
  N <- nrow(df$ddf$data)
  tr <- df[[1]]$meta.data$width
  # p <- df$ddf$fitted %>% unique()
  # esw <- round(p*tr, digits=3)
  esw <- round(predict(df, esw=TRUE)$fitted[1], digits=3)


  formula <- df$ddf$ds$aux$ddfobj$scale$formula
  formula <- if(is.null(formula)) "Uniform" else if(formula=="~1") "No covariates" else formula
  formula <- gsub("Clumped_","",formula)
  break_by <- tr/bins
  if(!is.null(bins)) break_specs <- seq(0,tr,break_by) else break_specs <- NULL

  plot(df,showpoints=showpoints, breaks = break_specs)
  abline(h=0.15, col="grey40", lty=2)
  if(details){
    usr <- par("usr")   # save old user/default/system coordinates
    par(usr = c(0, 1, 0, 1)) # new relative user coordinates
    text(x=0.95, y=0.85, label=paste("esw = ",esw, " km"),adj=c(1,1))
    text(x=0.95, y=0.8, label=paste("N = ", N),adj=c(1,1))
    text(x=0.95, y=0.9, label=paste(" ",formula),adj=c(1,1))
    text(x=0.95, y=0.75, label=paste("trunc = ",tr),adj=c(1,1))
    par(usr = usr) # restore original user coordinates
  }
  # if(details){
  # text(0.7*tr, 0.9, paste(key, adj, ord, ":", as.character(tr), " km", sep = " "))
  # }
  # title(paste(species), font.main = 1)
}
