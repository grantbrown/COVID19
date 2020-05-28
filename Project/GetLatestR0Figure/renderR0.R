library(rmarkdown)
setwd("/home/Analysis/GetLatestR0Figure/")
renderEnv <- new.env()
renderEnv$resultDirectory <- "/home/basefolder/Results/"
render("LatestR0Information.Rmd", envir = renderEnv)
infileName <- "LatestR0Information.html"
outfileName <- paste0(c("R0 Estimates - (", as.character(Sys.Date()), ").html"),
		      collapse = "")
outfileName <- paste0("/home/basefolder/Results/", outfileName)
file.copy(infileName, outfileName, overwrite = TRUE)


setwd("/home/Analysis/GetLatestR0Figure/")
renderEnv <- new.env()
renderEnv$resultDirectory <- "/home/basefolder/Results/"
render("LatestInformationAndProjections.Rmd", envir = renderEnv)
infileName <- "LatestInformationAndProjections.html"
outfileName <- paste0(c("Latest Projections - (", as.character(Sys.Date()), ").html"),
                      collapse = "")
outfileName <- paste0("/home/basefolder/Results/", outfileName)
file.copy(infileName, outfileName, overwrite = TRUE)