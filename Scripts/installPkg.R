args = commandArgs(trailingOnly = TRUE)

userLib <- Sys.getenv("R_LIBS_USER")
if (!dir.exists(userLib)){
	dir.create(userLib, recursive=TRUE)
}

Sys.setenv(MAKEFLAGS = "-j8")
options(Ncpus = 8)
for (pkg in args){
    install.packages(pkg,
                 repos = "https://cran.rstudio.com",
                 lib = userLib)


}

print(args)
