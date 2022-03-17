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

plot_df <- function(species, df, details=T, showpoints=T){
  # a <- df[[1]]$criterion %>% round(3) #AIC
  key <- df[[1]]$ds$aux$ddfobj$type
  key <- if(key=="hn") {"Half Normal"
  }else if(key=="hr") {"Hazard Rate"
  }else if(key=="un") {"Uniform"}
  # adj <- df[[1]]$ds$aux$ddfobj$adjustment$series
  # ord <- df[[1]]$ds$aux$ddfobj$adjustment$order %>% paste(collapse=",")
  N <- nrow(df$ddf$data)
  # tr <- df[[1]]$meta.data$width
  # p <- df$ddf$fitted %>% unique()
  # esw <- round(p*tr, digits=3)
  esw <- round(predict(df, esw=TRUE)$fitted[1], digits=3)


  formula <- df$ddf$ds$aux$ddfobj$scale$formula
  formula <- if(is.null(formula)) "Uniform" else if(formula=="~1") "No covariates" else formula

  plot(df,showpoints=showpoints)
  abline(h=0.15, col="grey40", lty=2)
  if(details){
    usr <- par("usr")   # save old user/default/system coordinates
    par(usr = c(0, 1, 0, 1)) # new relative user coordinates
    text(x=0.95, y=0.95, label=paste("esw = ",esw, " km"),adj=c(1,1),size=0.7)
    text(x=0.95, y=0.9, label=paste("N = ", N),adj=c(1,1),size=0.7)
    text(x=0.95, y=0.85, label=paste(" ",formula),adj=c(1,1),size=0.7)
    par(usr = usr) # restore original user coordinates
  }
  # if(details){
  # text(0.7*tr, 0.9, paste(key, adj, ord, ":", as.character(tr), " km", sep = " "))
  # }
  title(paste(sp), font.main = 1)
}
