userLib <- Sys.getenv("R_LIBS_USER")
dir.create(userLib, recursive=TRUE)
 
# Configure C++ toolchain
dotR <- file.path(Sys.getenv("HOME"), ".R")
if (!file.exists(dotR)) dir.create(dotR)
M <- file.path(dotR, "Makevars")
if (!file.exists(M)) file.create(M)
cat("\nCXX14FLAGS=-O3 -march=native -mtune=native -fPIC",
        "CXX14=g++", # or clang++ but you may need a version postfix
            file = M, sep = "\n", append = TRUE)


#system(paste("chown ", Sys.getenv("USER"), " ", Sys.getenv("R_LIBS_USER")))

Sys.setenv(MAKEFLAGS = "-j8")
options(Ncpus = 8)
for (pkg in c("fields", "mvtnorm", "dplyr", "Matrix", "knitr", 
              "splines", "coda", "rstan", "rmarkdown", "Rcpp", "RcppEigen", "igraph")){
    install.packages(pkg,
                 repos = "https://cran.rstudio.com",
                 lib = userLib)


}

