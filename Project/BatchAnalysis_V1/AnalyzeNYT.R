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
            default = "6", help = "Weeks to predict",
            metavar="character"),
make_option(c("-d", "--date"), type = "character", 
            default = "2020-01-01", help = "Intervention Date",
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
reopenDate <- as.Date("2020-05-01")
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
## Add Google Mobility data

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
  padData <- data.frame(date = max(stateDataFiltered$date) + 1:(padWeeks*7))
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
  data_model_1 = DataModel(stateDataFiltered$deathsNonCum,
                           type = "fractional",
                           compartment="R_star",
                           cumulative=FALSE, 
                           params = list(report_fraction = reportFraction, report_fraction_ess = 200))
  
  if (intervType == 1){
    X <- cbind(1, 1*(stateDataFiltered$date >= interventionDate), 1*(stateDataFiltered$date >= reopenDate)) # Weekly scale
  } else if (intervType == 2){
    X <- cbind(1, cumsum(stateDataFiltered$date >= interventionDate)/7, cumsum((stateDataFiltered$date >= reopenDate))/7) # Weekly scale
  } else if (intervType == 3){
    X <- cbind(1, 1*(stateDataFiltered$date >= interventionDate), 
               cumsum(stateDataFiltered$date >= interventionDate + 7)/7,
               cumsum((stateDataFiltered$date >= reopenDate))/7) # Weekly scale
  } else if (intervType == 4){
    c1 <- 1*((stateDataFiltered$date >= interventionDate+7)) 
    c2 <- 1*((stateDataFiltered$date >= reopenDate))
    X <- cbind(1, c1,c2)
  } else if (intervType == 5){
    c1 <- cumsum(1*((stateDataFiltered$date >= interventionDate)))
    
    sharedBasis <- bs(0:150, degree = 4) 
    X <- as.matrix(cbind(1,predict(sharedBasis,c1)))
    
  } else if (intervType == 6){
    # workplace mobility
    
    download.file(url = "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv?cachebust=6d352e35dcffafce",
                  destfile = "mobility.csv")
    
    mobility <- read.csv("mobility.csv")
    mobility <- filter(mobility, country_region == "United States" & sub_region_1 == "Iowa") %>% 
      group_by(date) %>%
      summarize(parks_mean = mean(parks_percent_change_from_baseline, na.rm=TRUE),
                retail_mean = mean(retail_and_recreation_percent_change_from_baseline, na.rm=TRUE), 
                transit_mean = mean(transit_stations_percent_change_from_baseline, na.rm=TRUE),
                residential_mean = mean(residential_percent_change_from_baseline, na.rm=TRUE),
                grocery_mean = mean(grocery_and_pharmacy_percent_change_from_baseline, na.rm=TRUE),
                workplace_mean = mean(workplaces_percent_change_from_baseline, na.rm=TRUE))
    
    # Throw away last few days of mobility
    mobility <- filter(mobility, as.Date(date) <= as.Date(max(date))-3)
    
    # Naiive assumption - assume that, moving forward, mobility stays at the previous week average. 
    X <- cbind(1, rep(NA, nrow(stateDataFiltered)))
    lastWkAvg <- mean(mobility$workplace_mean[order(mobility$date,decreasing = TRUE)][1:7])
    for (i in 1:nrow(X)){
      if (stateDataFiltered$date[i] %in% as.Date(mobility$date)){
        idx <- which(as.Date(mobility$date)==stateDataFiltered$date[i]) 
        X[i,2] <- mobility$workplace_mean[idx]
      } else{
        X[i,2] <- lastWkAvg
      }
    }
    
  } else if (intervType == 7){
    # Rec mobility
    
    download.file(url = "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv?cachebust=6d352e35dcffafce",
                  destfile = "mobility.csv")
    
    mobility <- read.csv("mobility.csv")
    mobility <- filter(mobility, country_region == "United States" & sub_region_1 == "Iowa") %>% 
      group_by(date) %>%
      summarize(parks_mean = mean(parks_percent_change_from_baseline, na.rm=TRUE),
                retail_mean = mean(retail_and_recreation_percent_change_from_baseline, na.rm=TRUE), 
                transit_mean = mean(transit_stations_percent_change_from_baseline, na.rm=TRUE),
                residential_mean = mean(residential_percent_change_from_baseline, na.rm=TRUE),
                grocery_mean = mean(grocery_and_pharmacy_percent_change_from_baseline, na.rm=TRUE),
                workplace_mean = mean(workplaces_percent_change_from_baseline, na.rm=TRUE))
    
    # Throw away last few days of mobility
    mobility <- filter(mobility, as.Date(date) <= as.Date(max(date))-3)
    
    
    # Naiive assumption - assume that, moving forward, mobility stays at the previous week average. 
    X <- cbind(1, rep(NA, nrow(stateDataFiltered)))
    lastWkAvg <- mean(mobility$retail_mean[order(mobility$date,decreasing = TRUE)][1:7])
    for (i in 1:nrow(X)){
      if (stateDataFiltered$date[i] %in% as.Date(mobility$date)){
        idx <- which(as.Date(mobility$date)==stateDataFiltered$date[i]) 
        X[i,2] <- mobility$retail_mean[idx]
      } else{
        X[i,2] <- lastWkAvg
      }
    }
    
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
                            samples = 50,
                            verbose = 2)
  
  
  simulated = epidemic.simulations(result, replicates = 2)
  tmp <- structure(list(modelObject = result, simulationResults = result$simulationResults, 
                        params = result$param.samples), class = "PosteriorSimulation")
  R0 <- ComputeR0(SimObject = simulated, cores = 1)
  R0post <- ComputeR0(SimObject = tmp, cores = 1)
  
  allSimulatedR0 = sapply(R0$simulationResults, function(x){x$R_EA})
  allSimulatedR0post = sapply(R0post$simulationResults, function(x){x$R_EA})
  
  single_state_results <- list(data = stateDataFiltered, 
                               result = R0post, 
                               sims = simulated, 
                               posterior_curves = tmp, 
                               post_pred_EAR = allSimulatedR0,
                               post_EAR = allSimulatedR0post,
                               location = instate
                               )
  save("single_state_results", file = cacheFileName, compress = "bzip2")
  
}


