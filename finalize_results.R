dt <- Sys.Date()
library(rmarkdown)
have_results <- length(dir("./artifacts")) > 0

if (have_results){
  rfldr <- paste0("./Results/", dt)
  if (file.exists(rfldr)){
    unlink(rfldr, recursive = TRUE)
  }
  dir.create(rfldr)
  
  rslt <- system(paste0("cp ./artifacts/* ", rfldr, " -r"))
  if (rslt == 0){
    unlink("./artifacts", recursive = TRUE)
    Sys.sleep(5)
    print(dir())
    dir.create("./artifacts")
  } else{
    stop("Failed to copy files")
  }
  rslt <- system(paste0("git add ", rfldr, "/*.html"))
  rslt <- rslt & system("git add ./Results/*.html")
  if (rslt != 0){stop("failed to add files")}
  rslt <- system(paste0("git commit -m \"Adding resuts for ", Sys.Date(), "\"")) 
  if (rslt != 0){stop("failed to commit files")}
  rslt <- system(paste0("git push origin master"))
}
