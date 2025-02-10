plot_ds.x <- function(x,
breaks=NULL,
bin.width=NULL,
nc=NULL,
# jitter.v=rep(0,3)
# showpoints=F
# subset=NULL
# pl.col="lightgrey"
# pl.den=NULL
# pl.ang=NULL
# main=NULL
# pages=0
pdf=FALSE,
ylim=NULL,
xlab="Distance (km)",
ylab = "Detection probability",
x.labs=NULL,
y.labs=NULL){

  model <- x #hw1.5.hn.v
  # model <-  hw1.5.hn.v #hp.0.65.hn.bfc.obs
  lower <- 0
vname <- "distance"
dat <- model$ddf$data
binwidth <- bin.width

width <- model$ddf$meta.data$width
left <- model$ddf$meta.data$left
ddfobj <- model$ddf$ds$aux$ddfobj
point <- model$ddf$ds$aux$point
max.range <- model$ddf$ds$aux$int.range

  # normalize <- FALSE
  # range.varies <- FALSE
  selected <- rep(TRUE, nrow(ddfobj$xmat))

  xmat <- ddfobj$xmat[selected,]
  z <- ddfobj$scale$dm[selected, , drop=FALSE]

  if(length(model$ddf$fitted)==1){
    pdot <- rep(model$ddf$fitted, sum(as.numeric(selected)))
  }else{
    pdot <- model$ddf$fitted[selected]
    Nhat <- sum(1/pdot)
  }

  zdim <- dim(z)[2]
  n <- length(xmat$distance)

  if(!is.null(breaks)){
    nc <- length(breaks)-1
  }

  if(is.null(nc)){
    nc <- round(sqrt(n), 0)
  }

  # Set logical hascov=TRUE when detection function has
  #  covariates other than distance and observer
  hascov <- FALSE
  if(!ddfobj$intercept.only){
    hascov <- TRUE
  }

  # Compute a grid for distance (xgrid), and covariates zgrid for
  # plotting of detection functions.
  if(!hascov){
    xgrid <- seq(0, width, length.out=101)
    zgrid <- matrix(rep(z[1,], length(xgrid)), byrow=TRUE, ncol=sum(zdim))
  }

  # Binning
  # create intervals of distance (breaks) for the chosen number of classes (nc).
  if(is.null(breaks)){
    if(is.null(model$ddf$meta.data$binned)){
      binned <- FALSE
    }else{
      binned <- model$ddf$meta.data$binned
    }
    if(binned){
      breaks <- model$ddf$ds$aux$breaks
      nc <- length(breaks)-1
    }else{
      breaks <- c(max(0, (max.range[1])),
                  max.range[1]+((max.range[2]-max.range[1])/nc)*(1:nc))
      if(breaks[1]>left){
        breaks <- c(left, breaks)
        nc <- nc+1
      }
    }
  }

  # test breaks for validity and reset as needed
  breaks <- mrds:::test.breaks(breaks, model$ddf$meta.data$left, width)
  nc <- length(breaks)-1
  lower <- min(breaks)
  upper <- max(breaks)
  dat <- dat[selected,]
  keep <- dat[ ,vname]>=lower & dat[ ,vname]<=upper

  # get the histogram object
  hist.obj <- hist(dat[ ,vname][keep], breaks=breaks, plot=FALSE)
# # what's the top of the largest bar?
# ymax <- max(hist.obj$counts)

  expected.counts <- (breaks[2:(nc+1)]-breaks[1:nc])*(Nhat/breaks[nc+1])

  hist.obj$density <- hist.obj$counts/expected.counts
  hist.obj$density[expected.counts==0] <- 0

  hist.obj$equidist <- FALSE

  ### Actual plotting starts here

  # area under the histogram
  hist_area <- sum(hist.obj$density*diff(breaks))

  # gives vector of detection probabilities length of obs within trunc dist
  point_vals <- detfct(xmat$distance, ddfobj, select=selected, width=width,
                       left=left)

  # set y labels, limits and tick marks (det.plot) depending on if we
  # are plotting PDF or df
  if(is.null(ylim)) ylim<-c(0, max(hist.obj$density, max(point_vals)))

  det.plot <- TRUE


  # histline() function:
  # hh <- list()
  # hh$breaks <- breaks
  # hh$counts <- hist.obj$density
  # hh$density <- hist.obj$density
  # hh$mids <- breaks[-length(breaks)] + diff(breaks)/2
  # hh$xname <- "hh"
  # hh$equidist <- FALSE
  # class(hh) <- "histogram"
  # box()

  # DATA FOR HISTOGRAM
  hh2 <- data.frame(hist.obj$density)
  names(hh2) <- "counts"
  hh2$mids <- breaks[-length(breaks)] + diff(breaks)/2
  mids <- breaks[-length(breaks)] + diff(breaks)/2
  # hh$xname <- "hh"
  # hh$equidist <- FALSE
  # class(hh) <- "histogram"

  # DATA FOR LINE
  # when we have covariates
  if(hascov){
    finebr <- seq(0, width, length.out=101)
    xgrid <- NULL
    linevalues <- NULL
    newdat <- xmat
    for(i in 1:(length(finebr)-1)){
      x <- (finebr[i]+finebr[i+1])/2
      xgrid <- c(xgrid, x)
      newdat$distance <- rep(x, nrow(newdat))

      detfct.values <- detfct(newdat$distance, ddfobj, select=selected,
                              width=width)

      linevalues <- c(linevalues, sum(detfct.values/pdot)/sum(1/pdot))

    }
    ## without covariates
  }else{
      if(!is.null(ddfobj$scale)){
        ddfobj$scale$dm <- ddfobj$scale$dm[rep(1, length(xgrid)), ,drop=FALSE]
      }
      if(!is.null(ddfobj$shape)){
        ddfobj$shape$dm <- ddfobj$shape$dm[rep(1, length(xgrid)), ,drop=FALSE]
      }

    linevalues <- detfct(xgrid, ddfobj, width=width, left=left)

    }

    # dots <- list()
    # dots$border <- NULL

    # plot the histogram
    # mrds:::histline(hist.obj$density, breaks=breaks, lineonly=FALSE,
    #                 xlab=xlab, ylab=ylab, ylim=ylim,
    #                 angle=angval1,
    #                 # density=denval1,
    #                 # col=pl.col,
    #                 det.plot=det.plot)
    #
    # # actually plot the lines
    # ldots <- dots
    # ldots$x <- xgrid
    # ldots$y <- linevalues
    # do.call(lines, ldots)

    dots <- data.frame(xgrid, linevalues)
    print(paste("max y = ", max(hh2$counts)))

    if(is.null(x.labs)) x.labs <- seq(0, width, width/5)
    if(is.null(y.labs)) y.labs <- seq(0, ylim[2], 0.2)
    if(is.null(bin.width)) bin.width <- width/nc

    # ggplot(dat)+geom_histogram(aes(x=distance), colour="grey30", fill="grey70", breaks = breaks)#+
    ggplot(hh2) +
      geom_col(aes(x=mids, y=counts), colour="grey30", fill="grey70", width = bin.width) +
      xlab(xlab) + ylab(ylab) +
      theme(panel.background = element_rect(colour="black",fill="white", linetype ="solid"),
            panel.border = element_rect(linetype ="solid", fill="transparent"), axis.line = element_line(),
            text=element_text(size=10))+
      scale_x_continuous(breaks=x.labs, labels=x.labs) +
      scale_y_continuous(breaks=y.labs, labels=y.labs) +
      geom_line(data=dots, aes(x=xgrid, y=linevalues)) +
      geom_line(data=dots, aes(x=xgrid, y=0.15), linetype=2, colour="grey40")


    }
# plot_ds.x(hw1.5.hn.v)
