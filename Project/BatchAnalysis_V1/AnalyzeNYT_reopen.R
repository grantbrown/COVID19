library(ABSEIR)
library(dplyr)
library(optparse)
library(splines)
library(openxlsx)


option_list <- list(
make_option(c("-s", "--state"), type = "character", 
            default = "1", help = "State to Analyze - index",
            metavar="character"),
make_option(c("-r", "--rate"), type = "character", 
            default = "0.02", help = "Assumed Mortality Rate",
            metavar="character"),
make_option(c("-w", "--weeks"), type = "character", 
            default = "12", help = "Weeks to predict",
            metavar="character"),
make_option(c("-d", "--date"), type = "character", 
            default = "2020-01-01", help = "Intervention Date",
            metavar="character"),
make_option(c("-p", "--reopendate"), type = "character", 
            default = "2020-05-01", help = "Date Intervention Relaxed",
            metavar="character"),
make_option(c("-f", "--reopenfraction"), type = "character", 
            default = "0.1", help = "0 to 1, ammount by which intervention effect is scaled. 0.1 means it shrinks by 10 percent",
            metavar="character"),
make_option(c("-t", "--type"), type = "character", 
            default = "3", help = "Intervention Type (1/2/3)",
            metavar="character"),
make_option(c("-o", "--outfile"), type = "character", 
            default = "tmp.rda", help = "output file",
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


reportFraction <- as.numeric(opt$rate)
padWeeks <- as.integer(opt$weeks)
state <- opt$state
interventionDate <- as.Date(opt$date)
reopenDate <- as.Date(opt$reopendate)
reopenFraction <- as.numeric(opt$reopenfraction)
intervType <- as.integer(opt$type)
cacheFileName <- opt$outfile
debug <- as.numeric(opt$debug) > 0
cacheFileName <- opt$outfile 
cores <- as.integer(opt$cores)



stateData <- read.csv("../Data/covid-19-data/us-states.csv", stringsAsFactors = FALSE)
censusData <- read.xlsx("../Data/nst-est2019-01.xlsx")
uqStates <- sort(unique(intersect(gsub(".", "", censusData$State, fixed = TRUE), stateData$state)))
instate <- uqStates[as.numeric(opt$state)]
stateCenIdx <- which(gsub(".", "", censusData$State, fixed = TRUE) == instate)

if (length(stateCenIdx) == 0){
  stop("State not found in Census Data: ", instate)  
}

statePop <- censusData[stateCenIdx,]$`2019`

if (file.exists(cacheFileName)){
  print("File Already Exists")
  
} else { 
  stateDataFiltered <- filter(stateData, state == instate) %>% arrange(date) %>%
    mutate(date = as.Date(date,format = "%Y-%m-%d")) %>% 
    mutate(idxDate = as.numeric(date - min(date))) %>%
    mutate(deathsNonCum = c(deaths[1], diff(deaths))) %>% 
    mutate(casesNonCum = c(cases[1], diff(cases))) %>%
    select(date, cases, deaths, idxDate, deathsNonCum, casesNonCum)
  
  minIdx <- max(1, min(which(stateDataFiltered$casesNonCum > 0))-7)
  stateDataFiltered <- stateDataFiltered[minIdx:nrow(stateDataFiltered),]
  
  #padWeeks
  maxObsDate <- max(stateDataFiltered$date)
  maxModelDate <- max(maxObsDate, reopenDate)
  
  if (maxModelDate > reopenDate){
    stop("Double check this code path before using!")
    padData <- data.frame(date = maxObsDate + 1:(padWeeks*7))
    stateDataFiltered <<- full_join(stateDataFiltered, padData, by = "date")
    stateDataFiltered$idxDate <- as.numeric(stateDataFiltered$date - min(stateDataFiltered$date))
  } else{
    padData <- data.frame(date = maxObsDate + 1:(maxModelDate-maxObsDate))
    stateDataFiltered <<- full_join(stateDataFiltered, padData, by = "date")
    stateDataFiltered$idxDate <- as.numeric(stateDataFiltered$date - min(stateDataFiltered$date))
  }
  
  
  # Fill in any zero counts
  nullData <- data.frame(idxDate = min(stateDataFiltered$idxDate):max(stateDataFiltered$idxDate), 
                         cases = NA, deaths = NA, deathsNonCum = 0, casesNonCum = 0)
  nullData <- nullData[!(nullData$idxDate %in% stateDataFiltered$idxDate),]
  if (nrow(nullData) > 0){
    stateDataFiltered <- full_join(stateDataFiltered, nullData, by = "idxDate")
  }
  
  # Build I and R data models
  data_model_1 = DataModel(stateDataFiltered$deathsNonCum,
                           type = "fractional",
                           compartment="R_star",
                           cumulative=FALSE, 
                           params = list(report_fraction = reportFraction, report_fraction_ess = 200))
  
  
  if (intervType != 6){
    warning("This code-path only handles intervention type 6")
  }
  c1 <- 1*((stateDataFiltered$date >= interventionDate+7))
  c2 <- 1*((stateDataFiltered$date >= reopenDate+7))
  X <- cbind(1, c1, c2)
  
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
                                             
  

  sampling_control = SamplingControl(seed = 12, 
                                     n_cores = cores,
                                     algorithm="Beaumont2009",
                                     list(init_batch_size = 2000000,
                                          batch_size = 1000,
                                          epochs = 1,
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
                            samples = 200,
                            verbose = 2)
  
  # This is a non-standard use of the package, so we have to monkey around a bit to get the forecasts
  
  # 1. Create copies of the result to do simulations with no reopening and with reopening
  
  result_reopen <- result 
  result_noreopen <- result
  # 2. Update the reopening parameter accordingly
  paramnames <- colnames(result$param.samples)
  result_reopen$param.samples[, which(paramnames == "Beta_SE_3")] <- -reopenFraction*result_reopen$param.samples[, which(paramnames == "Beta_SE_2")] 
  result_noreopen$param.samples[, which(paramnames == "Beta_SE_3")] <- 0
  
  # 3. Create new exposure and data models to match changing dimension, easiest to just reprocess data
  stateDataFiltered <- filter(stateData, state == instate) %>% arrange(date) %>%
    mutate(date = as.Date(date,format = "%Y-%m-%d")) %>% 
    mutate(idxDate = as.numeric(date - min(date))) %>%
    mutate(deathsNonCum = c(deaths[1], diff(deaths))) %>% 
    mutate(casesNonCum = c(cases[1], diff(cases))) %>%
    select(date, cases, deaths, idxDate, deathsNonCum, casesNonCum)
  
  minIdx <- max(1, min(which(stateDataFiltered$casesNonCum > 0))-7)
  stateDataFiltered <- stateDataFiltered[minIdx:nrow(stateDataFiltered),]
  
  #padWeeks
  offs <- (reopenDate - maxObsDate) 
  padData <- data.frame(date = maxObsDate + 1:(offs + padWeeks*7))
  stateDataFiltered <<- full_join(stateDataFiltered, padData, by = "date")
  stateDataFiltered$idxDate <- as.numeric(stateDataFiltered$date - min(stateDataFiltered$date))

  
  # Fill in any zero counts
  nullData <- data.frame(idxDate = min(stateDataFiltered$idxDate):max(stateDataFiltered$idxDate), 
                         cases = NA, deaths = NA, deathsNonCum = 0, casesNonCum = 0)
  nullData <- nullData[!(nullData$idxDate %in% stateDataFiltered$idxDate),]
  if (nrow(nullData) > 0){
    stateDataFiltered <- full_join(stateDataFiltered, nullData, by = "idxDate")
  }
  
  # Build I and R data models
  data_model_2 = DataModel(stateDataFiltered$deathsNonCum,
                           type = "fractional",
                           compartment="R_star",
                           cumulative=FALSE, 
                           params = list(report_fraction = reportFraction, report_fraction_ess = 200))
  
  
  c1 <- 1*((stateDataFiltered$date >= interventionDate+7))
  c2 <- 1*((stateDataFiltered$date >= reopenDate+7))
  X <- cbind(1, c1, c2)
  
  exposure_model_2 = ExposureModel(X,
                                 nTpt = nrow(stateDataFiltered),
                                 nLoc = 1,
                                 betaPriorPrecision = 1,
                                 betaPriorMean = 0)
  
  # 4. Insert the data and exposure models into the objects
  result_reopen$modelComponents$data_model <- data_model_2
  result_reopen$modelComponents$exposure_model <- exposure_model_2
  result_noreopen$modelComponents$data_model <- data_model_2
  result_noreopen$modelComponents$exposure_model <- exposure_model_2
  
    
  # 5. Get posterior predictive distributions (note, would be better to start from posterior, 
  #    but this should give an accurate picture of the expected effect)
  simulated_reopen = epidemic.simulations(result_reopen, replicates = 10)
  simulated_noreopen = epidemic.simulations(result_noreopen, replicates = 10)
  R0_reopen <- ComputeR0(SimObject = simulated_reopen, cores = 1)
  R0_noreopen <- ComputeR0(SimObject = simulated_noreopen, cores = 1)
  
  single_state_results <- list(data = stateDataFiltered, 
                               result_reopen = R0_reopen, 
                               result_noreopen = R0_noreopen,
                               location = instate)
  save("single_state_results", file = cacheFileName, compress = "bzip2")
  
}


