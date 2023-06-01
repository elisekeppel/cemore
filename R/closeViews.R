# from Mike McMahon through Ask a Guru on Teams
getRStudioOpenDocIDs <- function(showDets=F) {
  #' code largely taken from https://github.com/stevenjwest/projectmanagr/blob/master/R/RStudio-functions.R,
  #' but now works on windows (within Rstudio projects as well as with standalone instances of RStudio);
  #' and it returns a dataframe with some information about each of the tabs

  allDirs <- list.dirs(paste0(getwd(),"/.Rproj.user"), recursive = F)
  if(length(allDirs)>0){
    #if we're within an rstudio project, we'll be here
    allDirs <- allDirs[ !grepl("shared", allDirs) ]
    allDirs <- file.info(allDirs)
    rstudio_internal_state_dir <- rownames(allDirs)[order(allDirs$mtime)][nrow(allDirs)]
  }else{
    #not in an rstudio proj will be here
    rstudio_internal_state_dir <- paste0(Sys.getenv("LOCALAPPDATA"), .Platform$file.sep, "RStudio")
  }
  if( file.exists( paste0(rstudio_internal_state_dir, .Platform$file.sep, "sources") ) ) {
    if ( any(file.exists(  Sys.glob( paste0(rstudio_internal_state_dir, .Platform$file.sep, "sources", .Platform$file.sep, "session-*")  )  )  ) ){
      # get file list
      allSess <- Sys.glob( paste0(rstudio_internal_state_dir, .Platform$file.sep, "sources", .Platform$file.sep, "session-*")  )
      allSess <- file.info(allSess)
      recentSess <- rownames(allSess)[order(allSess$mtime)][nrow(allSess)]
      fileIDs <- list.files( recentSess  )
      # look in the "session-*" DIR for every file that does NOT end in "-contents"
      fileIDs <- fileIDs[lapply(fileIDs,function(x) length(grep("-contents",x,value=FALSE))) == 0]
      # nor have name "lock_file"
      fileIDs <- fileIDs[lapply(fileIDs,function(x) length(grep("lock_file",x,value=FALSE))) == 0]
      filePath <- paste0(recentSess, .Platform$file.sep, fileIDs)
      fileDf <- data.frame(ID=character(),
                           PATH = character(),
                           TYPE = character(),
                           CAPTION=character(),
                           TEMPNAME=character(),
                           ORDER = integer())
      for (i in 1:length(filePath) ) {
        fileDf[i,"ID"] <- fileIDs[i]
        FileConn <- file( filePath[i] )
        lines <- readLines( FileConn, warn = FALSE )
        close(FileConn)
        if (showDets) cat(lines,"\n\n")
        pathLine <- lines[grepl("    \"path\"*", lines)]
        typeLine <- lines[grepl("    \"type\"*", lines)]
        capLine <- lines[grepl("    \"caption\"*", lines)]
        tmpNameLine <- lines[grepl("    \"tempName\"*", lines)]
        if( grepl("\"path\" : null,", pathLine) || grepl("\"path\": null,", pathLine) ) {
          # path is null - file has not been saved - store "null" in fileList:
          fileDf[i,"PATH"] <- NA
        } else {
          # copy path into fileList - second index
          fileDf[i,"PATH"] <- substring(pathLine, regexpr(": \"", pathLine)+3, nchar(pathLine)-2)
        }
        if( grepl("\"type\" : null,", pathLine) || grepl("\"type\": null,", pathLine) ) {
          # path is null - file has not been saved - store "null" in fileList:
          fileDf[i,"TYPE"] <- NA
        } else {
          fileDf[i,"TYPE"] <- substring(typeLine, regexpr(": \"", typeLine)+3, nchar(typeLine)-2)
        }
        if(length(capLine)<1 || grepl("\"caption\" : null,", capLine) || grepl("\"caption\": null,", capLine) ) {
          # path is null - file has not been saved - store "null" in fileList:
          fileDf[i,"CAPTION"] <- NA
        } else {
          fileDf[i,"CAPTION"] <- substring(capLine, regexpr(": \"", capLine)+3, nchar(capLine)-2)
        }
        if(length(tmpNameLine)<1 || grepl("\"tmpName\" : null,", tmpNameLine) || grepl("\"tmpName\": null,", tmpNameLine) ) {
          # path is null - file has not been saved - store "null" in fileList:
          fileDf[i,"TEMPNAME"] <- NA
        } else {
          fileDf[i,"TEMPNAME"] <- substring(tmpNameLine, regexpr(": \"", tmpNameLine)+3, nchar(tmpNameLine)-2)
        }
        relOrderLine <- lines[grepl("    \"relative_order\"*", lines)]
        fileDf[i,"ORDER"] <- as.integer( substring(relOrderLine, regexpr("order", relOrderLine)+8, nchar(relOrderLine)-1) )
      }
    }
    else {
      # no files match pattern : paste0(rstudio_internal_state_dir, .Platform$file.sep, "sources", .Platform$file.sep, "session-*")
      stop( paste0("No Active RStudio Session") )
    }
  }
  fileDf <- fileDf[with(fileDf,order(ORDER)),]
  fileDf$ORDER <- NULL
  # return fileDf:
  fileDf
}
closeViews<-function(keepUntitledFiles= T){
  #' this function uses the dataframe from getRStudioOpenDocIDs to identify tabs opened by
  ##' View() as well as untitled tabs, and faciliates closing them
  openFiles<-getRStudioOpenDocIDs(showDets = F)
  views <- openFiles[openFiles$TYPE == "r_dataframe","ID"]
  untitled <- openFiles[is.na(openFiles$PATH) & !is.na(openFiles$TEMPNAME),"ID"]
  if (keepUntitledFiles){
    toClose <- c(views)
  }else{
    toClose <- c(views,untitled)
  }
  for (c in 1:length(toClose)){
    rstudioapi::documentClose(id=toClose[c], save = F)
  }
}
