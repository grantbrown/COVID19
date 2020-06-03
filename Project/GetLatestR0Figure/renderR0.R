library(rmarkdown)
setwd("/home/Analysis/GetLatestR0Figure/")

resultDirectory <- "../../Results/"
if (!dir.exists(resultDirectory)){
  resultDirectory <- "/home/basefolder/Results/"
}
resultDir <- setdiff(dir(resultDirectory), "placeholder")

## Determine if we're supposed to be working in a specific folder, or grabbing the latest
if (!exists("resultsDirectoryName")){
  maxidx <- which.max(as.Date(resultDir))
  rprtDate <- max(as.Date(resultDir), na.rm = TRUE)
  #resultsDirectoryName <- paste0(resultDirectory, resultDir[maxidx], "/results")
} else{
  rprtDate <- as.Date(resultsDirectoryName)
  #resultsDirectoryName <- paste0(resultDirectory, resultsDirectoryName, "/results")
}



renderEnv <- new.env()
renderEnv$resultDirectory <- "/home/basefolder/Results/"
render("LatestR0Information.Rmd", envir = renderEnv)
infileName <- "LatestR0Information.html"
outfileName <- paste0(c("R0 Estimates - (", as.character(rprtDate), ").html"),
		      collapse = "")
outfileName <- paste0("/home/basefolder/Results/", outfileName)
file.copy(infileName, outfileName, overwrite = TRUE)


setwd("/home/Analysis/GetLatestR0Figure/")
renderEnv <- new.env()
renderEnv$resultDirectory <- "/home/basefolder/Results/"
render("LatestInformationAndProjections.Rmd", envir = renderEnv)
infileName <- "LatestInformationAndProjections.html"
outfileName <- paste0(c("Latest Projections - (", as.character(rprtDate), ").html"),
                      collapse = "")
outfileName <- paste0("/home/basefolder/Results/", outfileName)
file.copy(infileName, outfileName, overwrite = TRUE)