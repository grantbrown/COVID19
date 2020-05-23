rslt <- system(paste0("git add index.html"))
if (rslt != 0){warning("failed to add index.html file")}
rslt <- system(paste0("git commit -m \"Updating index on ", Sys.Date(), "\"")) 
if (rslt != 0){warning("failed to commit index file")}
rslt <- system(paste0("git push origin master"))
if (rslt != 0){warning("failed to push index file")}
rslt <- system(paste0("git checkout gh-pages"),intern=TRUE)
print(rslt)
rslt <- system(paste0("git merge master -m \"Merge to gh-pages\""),intern=TRUE)
print(rslt)
rslt <- system(paste0("git push pub gh-pages"), intern = TRUE)
print(rslt)
rslt <- system(paste0("git checkout master"), intern = TRUE)
print(rslt)


