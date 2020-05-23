library(ABSEIR)
library(dplyr)
library(optparse)
library(splines)
library(openxlsx)


option_list <- list(
make_option(c("-s", "--state"), type = "character", 
              default = "", help = "State/Province Name",
              metavar="character"),
make_option(c("-c", "--country"), type = "character", 
            default = "China", help = "Country Name",
            metavar="character"),
make_option(c("-f", "--file"), type = "character", 
            default = "results.rda", help = "Out File Name",
            metavar="character"),
make_option(c("-r", "--rate"), type = "character", 
            default = "0.02", help = "Assumed Mortality Rate",
            metavar="character"),
make_option(c("-P", "--pop"), type = "character", 
            default = "58500000", help = "Population (total)",
            metavar="character"),
make_option(c("-w", "--weeks"), type = "character", 
            default = "0", help = "Weeks to pad at front",
            metavar="character"),
make_option(c("-d", "--date"), type = "character", 
            default = "2020-01-01", help = "Intervention Date",
            metavar="character"),
make_option(c("-t", "--type"), type = "character", 
            default = "3", help = "Intervention Type (1/2/3)",
            metavar="character"),
make_option(c("-p", "--pweeks"), type = "character", 
            default = "2", help = "Weeks to predict",
            metavar="character"),
make_option(c("-b", "--debug"), type = "character", 
            default = "0", help = "Debug?",
            metavar="character"),
make_option(c("-z", "--cores"), type = "character", 
            default = "8", help = "cores",
            metavar="character")

)



opt_parser <- OptionParser(option_list = option_list)
opt = parse_args(opt_parser)

# Grab input variables

state <- (opt$state)
country <- (opt$country)
reportFraction <- as.numeric(opt$rate)
statePop <- as.numeric(opt$pop)
padWeeks <- as.numeric(opt$weeks)
intervDate <- as.Date(opt$date)
intervType <- as.integer(opt$type)
predWeeks <- as.numeric(opt$pweeks)
cacheFileName <- opt$file
debug <- as.numeric(opt$debug) > 0
cores <- as.integer(opt$cores)

countryData <- read.csv("../Data/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
countryData2 <- read.csv("../Data/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")


DeathData <- filter(countryData, tolower(Country.Region)==tolower(country) & 
                      tolower(Province.State) == tolower(state))
CaseData <- filter(countryData, tolower(Country.Region)==tolower(country) & 
                       tolower(Province.State) == tolower(state))

if (nrow(DeathData) != 1 || nrow(CaseData) != 1){
  stop("Error, single country not found")
}

counts <- as.numeric(DeathData[1,5:ncol(DeathData)])
counts <- c(counts[1], diff(counts))


cases <- as.numeric(CaseData[1,5:ncol(CaseData)])
cases <- c(cases[1], diff(cases))


zeroPad <- function(x,n=2){
  paste0(paste0(rep("0", n-nchar(x)), collapse = ""), x)
}
dts <- sapply(strsplit(x = gsub("X", "", colnames(DeathData)[5:ncol(DeathData)]), 
                       split = ".", fixed = TRUE), function(x){
  paste0(zeroPad(x[1]), "-", zeroPad(x[2]), "-", zeroPad(x[3]), "20")
})

dts <- as.Date(dts, format = "%m-%d-%Y")

padDays <- padWeeks*7 
if (padDays == 0){
  padDays <- min(dts)
} else {
  padDays <- seq(min(dts) - padDays-1, min(dts)-1, by=1)
}
# Assume no undocumenteded deaths (reasonable?)
counts <- c(rep(0, length(setdiff(padDays, dts))), counts)
cases <- c(rep(NA, length(setdiff(padDays, dts))), cases)

dts <- sort(unique(c(padDays, dts)))


stateData <- data.frame(deathsNonCum = counts, date = dts)
if (predWeeks > 0){
  predData <- data.frame(date = max(stateData$date) + 1:(predWeeks*7))
  stateData <<- full_join(stateData, predData, by = "date")
}

intv <- 1*(stateData$date >= intervDate)

if (file.exists(cacheFileName)){
  # Do Nothing
  print("Cached results already exist.")
} else { 
  stateDataFiltered <- arrange(stateData,date) %>%
    mutate(idxDate = as.numeric(date - min(date))) %>%
    select(date, idxDate, deathsNonCum)
  
  
  # Build I and R data models
  data_model_1 = DataModel(stateDataFiltered$deathsNonCum,
                           type = "fractional",
                           compartment="R_star",
                           cumulative=FALSE, 
                           params = list(report_fraction = reportFraction, report_fraction_ess = 200))
  
  #X <- model.matrix(~bs(stateDataFiltered$idxDate, 
  #                      df = max(3, floor(nrow(stateDataFiltered)/21))))
  if (intervType == 1){
    X <- cbind(1, intv*1) # Weekly scale
  } else if (intervType == 2){
    X <- cbind(1, cumsum(intv)/7) # Weekly scale
  }else if (intervType == 3){
    X <- cbind(1, intv*1, cumsum(1*(stateData$date >= 7+intervDate)))
  }else if (intervType == 4){
    c1 <- 1*((stateData$date >= intervDate + 7)) #cumsum(1*((stateData$date >= intervDate) & (stateData$date < intervDate + 14)))/7
    #c2 <- cumsum(1*((stateData$date >= intervDate+7) & (stateData$date < intervDate + 14)))/7
    X <- cbind(1, c1)
  } else if (intervType == 5){
    c1 <- cumsum(1*((stateData$date >= intervDate)))
    
    sharedBasis <- bs(0:150, degree = 4) 
    X <- as.matrix(cbind(1,predict(sharedBasis,c1)))
    
  }

  exposure_model = ExposureModel(X,
                                 nTpt = nrow(stateDataFiltered),
                                 nLoc = 1,
                                 betaPriorPrecision = 1,
                                 betaPriorMean = 0)
  
  # There's no reinfection in this case, so we just use a "SEIR" model. 
  reinfection_model = ReinfectionModel("SEIR")
  
  # we have no distance model, because it's a non-spatial analysis
  distance_model = DistanceModel(list(matrix(0)))
  
  # Set initial population sizes
  initial_value_container = InitialValueContainer(S0=statePop,
                                                  E0=10,
                                                  I0=10,
                                                  R0=0, type = "uniform", 
                                                  params = list(max_S0 = statePop + 100,
                                                                max_E0 = 1000,
                                                                max_I0 = 1000,
                                                                max_R0 = 2))
  
  
  # Model to describe E to I and I to R transition probabilities.
  
  # Latent period: 2-14 days with median 5
  
  
  pickWeibullPars <- function(qdf){
    rslt <- optim(par = c(1,1), fn = function(par){
      sum((qweibull(p = qdf$q, shape = par[1], scale = par[2]) - qdf$x)^2)
    })
    rslt$par
  }
  
  pickGammaHyperPars <- function(mean, ESS){
    b <- ESS/(mean+1)
    a <- ESS - b
    c(a,b)
  }
  
  latent_par_means <- pickWeibullPars(qdf=data.frame(q=c(0.025,0.5,0.975),
                             x=c(2,5,14)))
  
  # 10-32 days infectious times, median 14 days. 
  infectious_par_means <- pickWeibullPars(qdf = data.frame(q=c(0.025,0.5,0.975),
                                                            x = c(10,14,32)))
  
  Transition_priors = WeibullTransitionPriors(latent_shape_prior_alpha = pickGammaHyperPars(latent_par_means[1], 1000)[1],
                                             latent_shape_prior_beta = pickGammaHyperPars(latent_par_means[1], 1000)[2],
                                             latent_scale_prior_alpha = pickGammaHyperPars(latent_par_means[2], 1000)[1],
                                             latent_scale_prior_beta = pickGammaHyperPars(latent_par_means[2], 1000)[2],
                                             infectious_shape_prior_alpha = pickGammaHyperPars(infectious_par_means[1], 100)[1],
                                             infectious_shape_prior_beta = pickGammaHyperPars(infectious_par_means[1], 100)[2],
                                             infectious_scale_prior_alpha = pickGammaHyperPars(infectious_par_means[2], 100)[1],
                                             infectious_scale_prior_beta = pickGammaHyperPars(infectious_par_means[2], 100)[2]) 
                                             
  # Set algorithm configuration
  
  sampling_control = SamplingControl(seed = 123124, 
                                     n_cores = cores,
                                     algorithm="Beaumont2009",
                                     list(init_batch_size = 1000000,
                                          batch_size = 100000,
                                          epochs = 2,
                                          max_batches = 2,
                                          shrinkage = 0.85,
                                          keep_compartments =TRUE
                                     ))
  
  
  result = SpatialSEIRModel(data_model_1,
                            exposure_model,
                            reinfection_model,
                            distance_model,
                            transition_priors = Transition_priors,
                            initial_value_container,
                            sampling_control,
                            samples = 100,
                            verbose = 2)
  
  simulated = epidemic.simulations(result, replicates = 2)
  tmp <- structure(list(modelObject = result, simulationResults = result$simulationResults, 
                        params = result$param.samples), class = "PosteriorSimulation")
  R0 <- ComputeR0(SimObject = simulated, cores = 1)
  R0post <- ComputeR0(SimObject = tmp, cores = 1)
  
  single_state_results <- list(data = stateDataFiltered, 
                               result = R0post, 
                               sims = R0,
                               location = state)
  save("single_state_results", file = cacheFileName, compress = "bzip2")
}

