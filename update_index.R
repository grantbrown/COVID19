rslt <- system(paste0("git add index.html"))
rslt <- rslt && system("git add ./Results/*.html")
if (rslt != 0){warning("failed to add index.html file")}
rslt <- system(paste0("git commit -m \"Updating index on ", Sys.Date(), "\"")) 
if (rslt != 0){warning("failed to commit index file")}
rslt <- system(paste0("git push origin master"))
if (rslt != 0){warning("failed to push index file")}
print(rslt)


