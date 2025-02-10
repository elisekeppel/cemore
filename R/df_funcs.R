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

plot_df <- function(species, df, details=T, showpoints=T,bins=NULL, qq=T, ylabel=NULL){
  # print("To resize bins, add argument 'bins = (an integer)' to plot_df()")
  # a <- df[[1]]$criterion %>% round(3) #AIC
  key <- df[[1]]$ds$aux$ddfobj$type
  key <- if(key=="hn") {"Half Normal" # {"HN" #
  }else if(key=="hr"){"Hazard Rate" #{"HR" #
  }else if(key=="un") {"Uniform"} # {"UN"} #
  # adj <- df[[1]]$ds$aux$ddfobj$adjustment$series
  # ord <- df[[1]]$ds$aux$ddfobj$adjustment$order %>% paste(collapse=",")
  n <- nrow(df$ddf$data)
  tr <- df[[1]]$meta.data$width
  # p <- df$ddf$fitted
  p <- nrow(df$ddf$data)/df$ddf$Nhat
  esw <- round(p*tr, digits=3)
  # esw <- round(predict(df, esw=TRUE)$fitted[1], digits=3)


  formula <- df$ddf$ds$aux$ddfobj$scale$formula
  formula <- if(is.null(formula)) "Uniform" else if(formula=="~1") "No covariates" else formula
  formula <- gsub("Clumped_","",formula)
  break_by <- tr/bins
  if(!is.null(bins)) break_specs <- seq(0,tr,break_by) else break_specs <- NULL

  plot(df,showpoints=showpoints, breaks = break_specs, xlab="Distance (km)", ylab=ylabel)
  abline(h=0.15, col="grey40", lty=2)
  if(details){
    usr <- par("usr")   # save old user/default/system coordinates
    par(usr = c(0, 1, 0, 1)) # new relative user coordinates
    text(x=0.96, y=0.98, label=paste(species),adj=c(1,1), cex= 25.4 / 72.27 * 3) # font=3, italics
    text(x=0.96, y=0.89, label=paste(formula),adj=c(1,1), cex= 25.4 / 72.27 * 3)
    # text(x=0.96, y=0.84, label=paste("Truncation = ",tr, " km"),adj=c(1,1), cex= 25.4 / 72.27 * 3)
    text(x=0.96, y=0.84, label=paste("esw = ",esw, " km"),adj=c(1,1), cex= 25.4 / 72.27 * 3)
    # text(x=0.96, y=0.79, label=paste("esw = ",esw, " km"),adj=c(1,1), cex= 25.4 / 72.27 * 3)
    text(x=0.96, y=0.79, label=paste("n = ", n),adj=c(1,1), cex= 25.4 / 72.27 * 3)
    par(usr = usr) # restore original user coordinates
  }
  # if(details){
  # text(0.7*tr, 0.9, paste(key, adj, ord, ":", as.character(tr), " km", sep = " "))
  # }
  # title(paste(species), font.main = 1)
}


get_h_t_abund <- function(mod, A=NULL, l=NULL){
  if(is.null(A)) A <- mod$dht$individuals$summary$Area
  if(is.null(l)) l <- mod$dht$individuals$summary$Effort
  w <- mod$ddf$meta.data$width
  a <- 2 * l * w

  group_sizes <- mod$ddf$data$size
  probs <- predict(mod)$fitted
  Nhat <- (A/a) * sum(group_sizes/probs)
  round(Nhat,0)
}

get_avg_p <- function(mod){
  summary(mod)$ds$average.p %>% round(4)
}
