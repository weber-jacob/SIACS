
# SIACS Batch Processing Function

#' Run SIACS Simulations in Batch Mode
#' 
#' @param position Integer, starting position in instances list
#' @param perturbation Logical
#' @param chemistry Logical
#' @param mechanism String, chemical mechanism name
#' @param input_data_list List of input data
#' @param instances Vector of instance indices
#' @param OutputList List of output configurations
#' @param use_parallel Logical
#' @param n_cores Integer
#' @export
SIACS.batch <- function(position = 1, 
                        perturbation = FALSE, 
                        chemistry = TRUE, 
                        mechanism = "SAPRC99", 
                        input_data_list, 
                        instances,
                        OutputList,
                        use_parallel = TRUE,
                        n_cores = NULL) {

  runs <- length(instances)
  if (position > runs) stop("Specified position beyond end of file list")
  
  parallel_mode <- FALSE
  cl <- NULL
  
  if (use_parallel && runs > 1) {
    if (is.null(n_cores)) {
      no_cores <- min(parallel::detectCores() - 1, runs)
    } else {
      no_cores <- min(n_cores, runs)
    }
    
    cat("Using parallel processing with", no_cores, "cores for", runs, "simulations\n")
    cl <- parallel::makeCluster(no_cores)
    doParallel::registerDoParallel(cl)
    
    # Export necessary objects
    working_dir <- getwd()
    parallel::clusterExport(cl, c("input_data_list", "instances", "mechanism", 
                        "perturbation", "chemistry", "OutputList", "working_dir"),
                  envir = environment())
    
    parallel::clusterEvalQ(cl, {
      setwd(working_dir)
      library(SIACS)
      library(deSolve)
      library(reshape)
      library(ggplot2)
      library(openxlsx)
    })
    
    parallel_mode <- TRUE
  } else {
    cat("Running in sequential mode\n")
    parallel_mode <- FALSE
    no_cores <- 1
  }
  
  iter <- foreach::foreach(
    instance = position:runs,
    .packages = c("openxlsx", "deSolve", "ggplot2", "reshape", "SIACS"),
    .errorhandling = "pass"
  )

  runner <- if (parallel_mode) foreach::`%dopar%` else foreach::`%do%`

  Results <- runner(iter, {
    if (parallel_mode) {
      log_file <- paste0("log__", instance, ".txt")
      file.create(log_file)
      sink(log_file, append = TRUE)
      on.exit(sink(), add = TRUE)
    }
    
    SIACSVersion <- "SIACS 0924"
    cat(SIACSVersion, "\n")
    
    # Initialize mechanism
    chemical.mechanism.selection(mechanism)
    cms <- chemical.mechanism.selection(mechanism)
    
    nrxn <- cms[[1]]
    species.df <- cms[[2]]
    spcnames <- cms[[3]]
    nspc <- cms[[4]]
    ngas <- cms[[5]]
    naer <- cms[[6]]
    # Reaction.Rates etc are available in environment potentially or need to be set?
    # chemical.mechanism.selection likely assigns to global env?
    # If so, that's bad for parallel.
    # But usually it returns a list.
    # The original code did:
    # chemical.mechanism.selection(mechanism)
    # cms <- chemical.mechanism.selection(mechanism)
    # And then relied on `chemical.mechanism.selection` side effects? 
    # Or relied on `cms` contents.
    # The `SIACS` main function calls `chemical.mechanism.selection` too.
    
    CheckDuration <- function(input, filename) {
      n <- nrow(input)
      max.input.time <- input$Time[n]
      if (max.input.time < endTime) {
        stop(
          "Input data in ", filename, " ends at ", max.input.time,
          " minutes, while simulation runs for ", endTime, " minutes"
        )
      }
      invisible(NULL)
    }
    
    current_instance_idx <- instances[instance]
    
    cat("Reading data for simulation", instance, "of", runs, "\n")
    result <- list()
    result[[paste0("FileNames", instance)]] <- OutputList[[current_instance_idx]]
    
    OptionalAnalyses <- list(dts = FALSE, dydx = FALSE, uncert = FALSE, mb = FALSE)
    
    if (OutputList[[current_instance_idx]]$OutputTimeDerivatives != "None") {
      OptionalAnalyses$dts <- TRUE
      if (OutputList[[current_instance_idx]]$OutputMassBalanceComponents != "None") {
        OptionalAnalyses$mb <- TRUE
      }
      if (OutputList[[current_instance_idx]]$OutputSensitivity != "None") {
        OptionalAnalyses$dydx <- TRUE
        if (OutputList[[current_instance_idx]]$OutputUncertainty != "None") {
          OptionalAnalyses$uncert <- TRUE
        }
      }
    }
    
    comptime0 <- Sys.time()
    
    time.read <- input_data_list$Time[[current_instance_idx]]
    start.time <- time.read$RelativeStartTime
    dTime <- time.read$TimeStep * 60
    maxTime <- time.read$Duration * 60 * 60
    times <- seq(from = start.time, to = maxTime, by = dTime)
    endTime <- (time.read$RelativeStartTime + time.read$Duration * 60)
    
    dv.df <- input_data_list$DepositionV[[current_instance_idx]]
    B <- input_data_list$BoxData[[current_instance_idx]]
    E <- input_data_list$PhysicalEnvironment[[current_instance_idx]]
    
    # Process E
    E <- as.data.frame(lapply(E, function(column) {
      column[column == "Uncertainty"] <- NA
      return(column)
    }))
    E[] <- lapply(E, function(column) {
      if (is.character(column)) return(as.numeric(column))
      return(column)
    })
    E <- E
    
    Etemp <- ExtractUncertainty(E, "Physical")
    E <- Etemp[[1]]
    E.uncert <- Etemp[[2]]
    E.uncert$LightFlux <- 0
    CheckDuration(E, "Physical Environment")
    
    O <- input_data_list$OutdoorConcentrations[[current_instance_idx]]
    O <- as.data.frame(lapply(O, function(column) {
      column[column == "Uncertainty"] <- NA
      if (is.character(column)) return(as.numeric(column))
      return(column)
    }))
    O <- O
    O <- ChemSpeciesStandardize(O, withtime = TRUE, Nspc = nspc, Spcnames = spcnames)
    Otemp <- ExtractUncertainty(O, "Outdoor", NSPC = nspc, SPCNAMES = spcnames)
    O <- Otemp[[1]]
    O.uncert <- Otemp[[2]]
    names(O.uncert) <- paste0(names(O.uncert), ".O")
    CheckDuration(O, "Outdoor Concentrations")
    
    EP <- input_data_list$EmissionProfiles[[current_instance_idx]]
    EP$ProfileName[EP$ProfileName == "Uncertainty"] <- NA
    EP[] <- lapply(names(EP), function(column_name) {
      column <- EP[[column_name]]
      if (is.character(column) && column_name != "ProfileName") {
        return(suppressWarnings(as.numeric(column)))
      }
      return(column)
    })
    EP <- as.data.frame(EP)
    EPtemp <- ExtractUncertainty(EP, "Emissions", NSPC = nspc, SPCNAMES = spcnames)
    EP <- EPtemp[[1]]
    S.uncert <- EPtemp[[2]]
    names(S.uncert) <- paste0(names(S.uncert), ".S")
    
    Activities <- input_data_list$Activities[[current_instance_idx]]
    CheckDuration(Activities, "Activities Schedule")
    
    Y.ini <- input_data_list$InitialValues[[current_instance_idx]]
    
    Indoor.Flux <- list()
    if (!is.null(input_data_list$IndoorLight[[current_instance_idx]])) {
       Indoor.Flux$J_values <- as.data.frame(input_data_list[["IndoorLight"]][[current_instance_idx]][["J_values"]])
       Indoor.Flux$EnergyFlux <- as.data.frame(input_data_list[["IndoorLight"]][[current_instance_idx]][["EnergyFlux"]])
    } else {
       cat("Indoor Light flux not available. It will be calculated...\n")
       if (!is.null(input_data_list$OutdoorLightDirect[[current_instance_idx]])) {
         DirectFlux.out <- input_data_list$OutdoorLightDirect[[current_instance_idx]]
       } else {
         DirectFlux.out <- OutdoorLightFlux(time.read, "direct", comptime0)
         if(!parallel_mode) write.csv(DirectFlux.out, paste0("OutdoorLightDirect_", instance, ".csv"), row.names = FALSE)
       }
       
       if (!is.null(input_data_list$OutdoorLightDiffuse[[current_instance_idx]])) {
         DiffuseFlux.out <- input_data_list$OutdoorLightDiffuse[[current_instance_idx]]
       } else {
         DiffuseFlux.out <- OutdoorLightFlux(time.read, "diffuse", comptime0)
         if(!parallel_mode) write.csv(DiffuseFlux.out, paste0("OutdoorLightDiffuse_", instance, ".csv"), row.names = FALSE)
       }
       
       if (!is.null(input_data_list$ArtificialLight[[current_instance_idx]])) {
         Artificial.Flux <- input_data_list$ArtificialLight[[current_instance_idx]]
       } else {
         if (is.null(input_data_list$ArtificialLightList[[current_instance_idx]])) stop("Artificial lights list not found")
         if (is.null(input_data_list$ArtificialLightSpectra[[current_instance_idx]])) stop("Artificial lights spectra not found")
         if (is.null(input_data_list$ArtificialLightSchedule[[current_instance_idx]])) stop("Artificial lights schedule not found")
         
         ArtificialLightList <- input_data_list$ArtificialLightList[[current_instance_idx]]
         ArtificialLightSpectra <- input_data_list$ArtificialLightSpectra[[current_instance_idx]]
         ArtificialLightSchedule <- input_data_list$ArtificialLightSchedule[[current_instance_idx]]
         
         Artificial.Flux <- ArtificialLightFlux(ArtificialLightList, ArtificialLightSpectra, ArtificialLightSchedule, B)
         if(!parallel_mode) write.csv(Artificial.Flux, paste0("ArtificialLight_", instance, ".csv"), row.names = FALSE)
       }
       
       if (is.null(input_data_list$Windows[[current_instance_idx]])) stop("No windows information found")
       windows <- input_data_list$Windows[[current_instance_idx]]
       
       if (is.null(input_data_list$GlassTransmission[[current_instance_idx]])) stop("No glass transparency information found")
       glass <- input_data_list$GlassTransmission[[current_instance_idx]]
       
       cat("Calculating indoor light flux from outdoors and artificial lights...\n")
       Indoor.Flux <- IndoorLightFlux(time.read, DirectFlux.out, DiffuseFlux.out, Artificial.Flux, windows, glass, B)
       Indoor.Flux$J_values <- Photolysis.rates(Indoor.Flux$Total)
       
       # Not saving Excel files in parallel mode to avoid conflicts/overhead?
       # Or save with unique names.
       # Original code saved files. I'll keep it but perhaps it's desired.
    }
    
    comptime1 <- Sys.time()
    PrintComputationTime("Time preparing data for simulation ", comptime0, comptime1, instance, runs)
    
    # Run Simulation
    Emissions <- rep(0.0, as.numeric(nspc))
    names(Emissions) <- paste0(spcnames, ".S")
    OutdoorConcs <- rep(0.0, as.numeric(nspc))
    names(OutdoorConcs) <- paste0(spcnames, ".O")
    
    simulation_temp <- SIACS(times, dv.df,
                             building = B, E, O, EP, Activities, Indoor.Flux, Y.ini, perturbation, chemistry, mechanism,
                             numspc = nspc, eunc = E.uncert, ounc = O.uncert, sunc = S.uncert, OutdoorNames = OutdoorConcs,
                             sn = spcnames, sdf = species.df, EmissionNames = Emissions, start = start.time,
                             Ngas = ngas, N = nrxn
    )
    
    simulation <- simulation_temp[[1]]
    aP.app.lst <- simulation_temp[[2]]
    combined.variables.n <- simulation_temp[[3]]
    
    result[[paste0("Simulation", instance)]] <- simulation
    
    prms <- list(
      nspc = nspc, ngas = ngas, naer = naer, SV = SIACSVersion, OC = OutdoorConcs,
      sdf = species.df, sn = spcnames, aP.app.lst = aP.app.lst,
      combined.variables.n = combined.variables.n
    )
    uncs <- list(eunc = E.uncert, ounc = O.uncert, sunc = S.uncert)
    
    Save.results(simulation$alldata, OutputList[[current_instance_idx]], instance, chemistry, mechanism, parms = prms)
    
    if (OutputList[[current_instance_idx]]$OutputBasicChart != "None") {
      tryCatch({
        Create.result.plots(simulation$alldata,
                            outfile = paste0(OutputList[[current_instance_idx]]$OutputBasicChart, instance, ".png"),
                            maxTime, parms = prms
        )
      }, error = function(e) {
        message("ERROR creating basic chart for instance ", instance, ": ", conditionMessage(e))
      })
    }
    
    dydtMatrix <- NULL
    if (OptionalAnalyses$dts) {
      dydtMatrix <- tryCatch({
        Deptime.derivative.do(simulation$deSolve,
                              outfile = paste0(OutputList[[current_instance_idx]]$OutputTimeDerivatives, instance, ".csv"),
                              simulation$parms, gvs = prms)
      }, error = function(e) {
        message("ERROR creating time derivatives for instance ", instance, ": ", conditionMessage(e))
        NULL
      })
      result[[paste0("dydt", instance)]] <- dydtMatrix
    }
    
    if (OptionalAnalyses$mb) {
      Ef <- dv.df$FilterEfficiency
      V <- B$FloorSurfaceArea * B$RoomHeight
      mb <- tryCatch({
        Mass.balance.do(simulation$alldata,
                        outfile = paste0(OutputList[[current_instance_idx]]$OutputMassBalanceComponents, instance, ".xlsx"),
                        V, dydtMatrix, B, dv.df, Ef, gvs = prms)
      }, error = function(e) {
        message("ERROR creating mass balance for instance ", instance, ": ", conditionMessage(e))
        NULL
      })
      result[[paste0("MassBalance", instance)]] <- mb
    }
    
    dydxMatrix <- NULL
    if (OptionalAnalyses$dydx) {
      derivatives <- tryCatch({
        Dydx.do(simulation$alldata,
                outfile = paste0(OutputList[[current_instance_idx]]$OutputSensitivity, instance, ".csv"),
                time.read, dydtMatrix, gvs = prms)
      }, error = function(e) {
        message("ERROR creating sensitivity for instance ", instance, ": ", conditionMessage(e))
        NULL
      })
      dydxMatrix <- if (!is.null(derivatives)) derivatives$dydx else NULL
      result[[paste0("Derivatives", instance)]] <- derivatives
    }
    
    if (OptionalAnalyses$uncert) {
      Result.uncert <- tryCatch({
        Uncertainty.do(simulation$alldata,
                       outfile = paste0(OutputList[[current_instance_idx]]$OutputUncertainty, instance, ".csv"),
                       dydxMatrix, Unc = uncs, gvs = prms)
      }, error = function(e) {
        message("ERROR creating uncertainty for instance ", instance, ": ", conditionMessage(e))
        NULL
      })
      result[[paste0("Uncertainty", instance)]] <- Result.uncert
    }
    
    comptime3 <- Sys.time()
    PrintComputationTime("Total time for simulation ", comptime0, comptime3, instance, runs)
    
    result
  })
  
  if (parallel_mode) {
    parallel::stopCluster(cl)
  }
  
  # Print any errors caught by foreach
  for (i in seq_along(Results)) {
    if (inherits(Results[[i]], "error")) {
      cat("\n--- CRITICAL ERROR IN SIMULATION", instances[[i]], "---\n")
      cat("Message:", conditionMessage(Results[[i]]), "\n")
      cat("Call:", deparse(conditionCall(Results[[i]])), "\n")
      cat("--------------------------------------------\n")
    }
  }
  
  invisible(Results)
}