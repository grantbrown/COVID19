library(rmarkdown)

rd <- "../../Results/"
dts <- as.character(na.omit(as.Date(dir(rd))))

for (dt in dts){
  print(dt)
  renderEnv <- new.env()
  renderEnv$resultsDirectoryName <- as.character(dt)  
  infileName <- "LatestInformationAndProjections.html"
  render("LatestInformationAndProjections.Rmd", envir = renderEnv)
  outfileName <- paste0(c("Latest Projections - (", as.character(dt), ").html"),
                        collapse = "")
  outfileName <- paste0(rd, outfileName)
  file.copy(infileName, outfileName,overwrite = TRUE)
  file.remove(infileName)
  
}
