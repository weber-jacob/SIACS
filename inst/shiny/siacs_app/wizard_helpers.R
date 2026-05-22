# wizard_helpers.R
# =======================================================================
# Helper functions for SIACS Wizard
# =======================================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

# Find a file in the Input/ directory by partial name match (same logic as
# wizard_defaults find_path but usable at assembly time without file_paths_advanced)
find_input_file <- function(pattern) {
  candidates <- list.files("Input", full.names=TRUE, ignore.case=TRUE)
  hits <- candidates[grepl(pattern, basename(candidates), ignore.case=TRUE)]
  if (length(hits) > 0) hits[1] else NULL
}

normalize_output_path <- function(x) {
  if (is.null(x)) return(NULL)
  x <- trimws(as.character(x))
  if (!nzchar(x)) return(NULL)
  if (identical(x,"None")) return("None")
  if (grepl("^\\./"  ,x)) return(x)
  if (grepl("^Output/",x)) return(paste0("./",x))
  return(x)
}

wizard_output_defaults <- function() {
  if (exists("get_default_output_values")) return(as.list(get_default_output_values()))
  list(
    OutputTable                 = "./Output/ATL-CMAQ for Ambient",
    OutputBasicChart            = "./Output/ATL-CMAQ for Ambient",
    OutputTimeDerivatives       = "./Output/ATL-CMAQ for Ambient Derivatives",
    OutputMassBalanceComponents = "./Output/ATL-CMAQ for Ambient Mass Balance",
    OutputSensitivity           = "./Output/ATL-CMAQ for Ambient Sensitivity",
    OutputUncertainty           = "./Output/ATL-CMAQ for Ambient uncertainty"
  )
}

# -----------------------------------------------------------------------
# INSERT STEP-CHANGE HELPERS
# Used by Screen 8 (light schedule), Screen 9 (phys env), Screen 10 (activities)
# -----------------------------------------------------------------------

# Wide-format step change: inserts a row at (t - trans) with the CURRENT
# value of `col`, and a row at t with `new_val`.  Works for both the
# light schedule (many light columns) and the physical environment
# schedule (many variable columns).
insert_step_change_wide <- function(df, col, t, new_val, trans = 0.1) {
  if (!(col %in% names(df))) {
    warning("Column '", col, "' not found in schedule. Skipping.")
    return(df)
  }
  df <- df[order(df$Time), ]

  # Value of col just before the event (last row with Time <= t)
  before_rows <- df[df$Time <= t, ]
  prev_val    <- if (nrow(before_rows) > 0) tail(before_rows[[col]], 1) else 0

  # Pre-event row (only if t > 0; a transition before t=0 is meaningless)
  last_before   <- if (nrow(before_rows) > 0) tail(before_rows, 1) else df[1, ]

  # Event row
  new_at        <- last_before
  new_at$Time   <- t
  new_at[[col]] <- new_val

  if (t > 0) {
    t_pre          <- t - max(trans, 0.001)
    new_pre        <- last_before
    new_pre$Time   <- t_pre
    new_pre[[col]] <- prev_val   # unchanged value right before the jump
    df <- rbind(df, new_pre, new_at)
    df <- df[order(df$Time), ]
    df <- df[!duplicated(df$Time), ]
  } else {
    # Update the existing t=0 row in-place so deduplication doesn't drop the new value
    df[df$Time == 0, col] <- new_val
  }
  rownames(df) <- NULL
  df
}

# Two-column (Time / Value) step change — for the activity schedule table.
insert_step_change_two_col <- function(df, t, new_val, trans = 0.1) {
  df   <- df[order(df$Time), ]
  before <- df[df$Time <= t, ]
  prev_val <- if (nrow(before) > 0) tail(before$Value, 1) else 0

  # Only insert a pre-event transition row if t > 0; subtracting transition
  # time from t=0 would produce a negative time, which is meaningless.
  if (t > 0) {
    t_pre <- t - max(trans, 0.001)
    df <- rbind(df,
      data.frame(Time = t_pre, Value = prev_val, stringsAsFactors = FALSE),
      data.frame(Time = t,     Value = new_val,  stringsAsFactors = FALSE))
    df <- df[order(df$Time), ]
    df <- df[!duplicated(df$Time), ]
  } else {
    # Update the existing t=0 row in-place so deduplication doesn't drop the new value
    df[df$Time == 0, "Value"] <- new_val
  }
  rownames(df) <- NULL
  df
}

# -----------------------------------------------------------------------
# FORWARD-FILL HELPER
# After a row is deleted or a new event row is inserted, some cells may be
# NA (rhandsontable returns NA for cells the user never touched).  This
# function replaces every NA in non-Time columns with the value from the
# last row *above* that position which had a fully-populated value for that
# column.  It also removes any rows where the Time cell is NA (blank rows
# that rhandsontable can create when a row is right-click-deleted).
# -----------------------------------------------------------------------
fill_schedule_gaps <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(df)

  # Drop rows where Time is NA (blank rows left by rhandsontable delete)
  df <- df[!is.na(df$Time), , drop = FALSE]
  if (nrow(df) == 0) return(df)

  df <- df[order(df$Time), ]
  rownames(df) <- NULL

  non_time_cols <- setdiff(names(df), "Time")

  for (col in non_time_cols) {
    for (i in seq_len(nrow(df))) {
      if (is.na(df[i, col])) {
        # Find the nearest complete value above
        above <- which(!is.na(df[seq_len(i - 1), col]))
        if (length(above) > 0) {
          df[i, col] <- df[max(above), col]
        }
        # If still NA (nothing above), look below
        if (is.na(df[i, col])) {
          below <- which(!is.na(df[seq(i + 1, nrow(df)), col]))
          if (length(below) > 0) {
            df[i, col] <- df[i + below[1], col]
          }
        }
      }
    }
  }
  df
}

# -----------------------------------------------------------------------
# LEGACY event-based generators (still used when assembling from the old
# per-variable phys_events data.frame, and for backward compatibility)
# -----------------------------------------------------------------------
generate_timeseries_from_events <- function(events, duration_minutes,
                                            initial_value, final_value,
                                            transition_time = 0.1,
                                            variable_name = "Value") {
  times  <- c(0, duration_minutes)
  values <- c(initial_value, final_value)

  if (!is.null(events) && nrow(events) > 0) {
    for (i in seq_len(nrow(events))) {
      et <- events$Time[i]; ev <- events$Value[i]
      times  <- c(times, max(0, et - transition_time))
      values <- c(values, values[length(values)])
      times  <- c(times, et)
      values <- c(values, ev)
    }
  }

  ord  <- order(times); times <- times[ord]; values <- values[ord]
  keep <- !duplicated(times); times <- times[keep]; values <- values[keep]

  result <- data.frame(Time = times)
  result[[variable_name]] <- values
  result
}

generate_light_schedule <- function(light_events, num_lights, duration_minutes,
                                    transition_time = 0.1) {
  schedule <- data.frame(Time = c(0, duration_minutes))
  for (i in seq_len(num_lights)) schedule[[paste0("Light",i)]] <- 0

  if (is.null(light_events) || nrow(light_events) == 0) return(schedule)

  for (i in seq_len(num_lights)) {
    ln   <- paste0("Light",i)
    sub  <- light_events[light_events$Light == ln, ]
    if (nrow(sub) > 0) {
      ts <- generate_timeseries_from_events(
        data.frame(Time=sub$Time, Value=sub$Power),
        duration_minutes, 0, 0, transition_time, ln)
      schedule <- merge(schedule, ts, by="Time", all=TRUE)
      schedule[[paste0(ln,".x")]] <- NULL
      names(schedule)[names(schedule) == paste0(ln,".y")] <- ln
    }
  }
  for (col in names(schedule)[-1]) {
    schedule[[col]] <- zoo::na.locf(schedule[[col]], na.rm=FALSE)
    schedule[[col]][is.na(schedule[[col]])] <- 0
  }
  schedule[order(schedule$Time), ]
}

generate_physical_environment <- function(phys_events, duration_minutes,
                                          initial_values, final_values,
                                          transition_time = 0.1) {
  all_vars <- c("Ti","To","OpenWindowArea","QBal","QUnbal","QFilter","RH","BP","Wind")
  schedule <- data.frame(Time = c(0, duration_minutes))
  for (var in all_vars) {
    ev  <- if (!is.null(phys_events) && nrow(phys_events) > 0)
      phys_events[phys_events$Variable == var, ] else data.frame(Time=numeric(0),Value=numeric(0))
    ts  <- generate_timeseries_from_events(
      data.frame(Time=ev$Time, Value=ev$Value),
      duration_minutes, initial_values[[var]], final_values[[var]],
      transition_time, var)
    schedule <- merge(schedule, ts, by="Time", all=TRUE)
  }
  for (col in names(schedule)[-1]) schedule[[col]] <- zoo::na.locf(schedule[[col]], na.rm=FALSE)
  schedule[order(schedule$Time), ]
}

generate_activities_schedule <- function(activity_schedules, duration_minutes,
                                         transition_time = 0.1) {
  all_activities <- c("Generic","Adult","Smoking","GasCooking.Persily1998","Incense.Manoukian2013")
  schedule <- data.frame(Time = c(0, duration_minutes))
  for (act in all_activities) {
    events <- activity_schedules[[act]]
    if (!is.null(events) && nrow(events) > 0) {
      init_val  <- if (events$Time[1] == 0) events$Value[1] else 0
      final_val <- events$Value[nrow(events)]
      ts <- generate_timeseries_from_events(events, duration_minutes,
        init_val, final_val, transition_time, act)
      schedule <- merge(schedule, ts, by="Time", all=TRUE)
    } else {
      schedule[[act]] <- 0
    }
  }
  for (col in names(schedule)[-1]) {
    schedule[[col]] <- zoo::na.locf(schedule[[col]], na.rm=FALSE)
    schedule[[col]][is.na(schedule[[col]])] <- 0
  }
  schedule[order(schedule$Time), ]
}

write_csv_with_units <- function(data, filepath, units_row = NULL) {
  if (!is.null(units_row)) {
    units_line <- paste0("# ", paste(units_row, collapse = ","))
    writeLines(units_line, filepath)
    suppressWarnings(
      write.table(data, filepath, append = TRUE, sep = ",",
                  row.names = FALSE, quote = FALSE)
    )
  } else {
    write.csv(data, filepath, row.names = FALSE, quote = FALSE)
  }
  cat("Written:", filepath, "\n")
}

# -----------------------------------------------------------------------
# ASSEMBLE AND WRITE WIZARD DATA
# -----------------------------------------------------------------------
assemble_and_write_wizard_data <- function(input, wizard_state, output_dir = NULL) {

  # Local coefficient tables (fallback when wiz_stack_coeff not present)
  stack_coeff_table <- data.frame(
    stories=1:3,
    two=c(0.000420,0.000326,0.000231))
  wind_coeff_table <- data.frame(
    stories=1:3,
    two=c(0.000494,0.000382,0.000271))

  cat("\n=== ASSEMBLING AND WRITING WIZARD DATA ===\n")

  if (is.null(output_dir)) {
    output_dir <- paste0("Input_", format(Sys.time(),"%Y-%m-%d_%H%M%S"))
  }
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive=TRUE)
    cat("Created directory:", output_dir, "\n")
  }

  files_created <- list()
  assembly_errors   <- character(0)
  assembly_warnings <- character(0)

  # Helper: run a validate_* function and collect into assembly results
  run_assembly_check <- function(df, type, label, duration_hours = NULL) {
    res <- tryCatch(
      validate_data_file(df, type, duration_hours),
      error = function(e) create_validation_result(TRUE, character(0),
        paste("Could not validate", label, ":", e$message))
    )
    if (length(res$errors)   > 0)
      assembly_errors   <<- c(assembly_errors,   paste0("[", label, "] ", res$errors))
    if (length(res$warnings) > 0)
      assembly_warnings <<- c(assembly_warnings, paste0("[", label, "] ", res$warnings))
    invisible(res)
  }

  # Ensure global lists exist
  if (!exists("input_data_list",envir=.GlobalEnv)) assign("input_data_list",list(),envir=.GlobalEnv)
  input_data_list <- get("input_data_list",envir=.GlobalEnv)
  if (!exists("OutputList",envir=.GlobalEnv))      assign("OutputList",list(),envir=.GlobalEnv)
  OutputList <- get("OutputList",envir=.GlobalEnv)
  if (!exists("instances",envir=.GlobalEnv))       assign("instances",c(1),envir=.GlobalEnv)
  instances <- get("instances",envir=.GlobalEnv)

  current_idx <- if (length(instances) > 0) tail(as.integer(instances),1) else 1
  if (is.na(current_idx) || current_idx < 1) current_idx <- 1

  # Helper: build a path inside the per-instance output_dir.
  # Files are no longer suffixed — each instance has its own folder.
  inp_path <- function(filename) {
    file.path(output_dir, filename)
  }

  # -------------------------------------------------------------------
  # 1) Seed all 17 default files into current_idx slot
  # -------------------------------------------------------------------
  read_default_file <- function(filepath) {
    if (!file.exists(filepath)) return(NULL)
    if (grepl("\\.xlsx$",filepath,ignore.case=TRUE)) {
      if (!requireNamespace("readxl",quietly=TRUE)) return(NULL)
      tryCatch({
        sheets <- readxl::excel_sheets(filepath)
        if (length(sheets)==1) as.data.frame(readxl::read_excel(filepath))
        else { df_list <- lapply(sheets,function(s) as.data.frame(readxl::read_excel(filepath,sheet=s))); names(df_list)<-sheets; df_list }
      },error=function(e) NULL)
    } else {
      tryCatch(read.csv(filepath,comment.char="#",stringsAsFactors=FALSE),error=function(e) NULL)
    }
  }

  cat("Loading default files into input_data_list index",current_idx,"...\n")
  for (i in seq_along(titles_advanced)) {
    file_key  <- gsub(" ","",titles_advanced[i])
    file_path <- file_paths_advanced[i]
    if (is.null(input_data_list[[file_key]])) input_data_list[[file_key]] <- list()
    if (file_path != "none") {
      df <- read_default_file(file_path)
      if (!is.null(df)) input_data_list[[file_key]][[current_idx]] <- df
    }
  }

  # -------------------------------------------------------------------
  # 2) OutputList
  # -------------------------------------------------------------------
  adv <- wizard_output_defaults()
  out <- adv
  out$OutputTable       <- normalize_output_path(input$wiz_output_table) %||% adv$OutputTable
  out$OutputBasicChart  <- normalize_output_path(input$wiz_output_chart) %||% adv$OutputBasicChart
  out$OutputTimeDerivatives       <- if (isTRUE(input$wiz_output_derivatives))
    normalize_output_path(input$wiz_output_derivatives_file) %||% adv$OutputTimeDerivatives else "None"
  out$OutputMassBalanceComponents <- if (isTRUE(input$wiz_output_massbalance))
    normalize_output_path(input$wiz_output_massbalance_file) %||% adv$OutputMassBalanceComponents else "None"
  out$OutputSensitivity           <- if (isTRUE(input$wiz_output_sensitivity))
    normalize_output_path(input$wiz_output_sensitivity_file) %||% adv$OutputSensitivity else "None"
  out$OutputUncertainty           <- if (isTRUE(input$wiz_output_uncertainty))
    normalize_output_path(input$wiz_output_uncertainty_file) %||% adv$OutputUncertainty else "None"

  OutputList[[current_idx]] <- out
  assign("OutputList",OutputList,envir=.GlobalEnv)

  # -------------------------------------------------------------------
  # 3) Overwrite with wizard-generated data
  # -------------------------------------------------------------------
  update_input_list <- function(key, data) {
    # Use <<- to modify input_data_list in the enclosing function scope,
    # not create a local copy inside this closure. Without <<- every call
    # modifies a throw-away local binding and the changes are lost.
    if (is.null(input_data_list[[key]])) input_data_list[[key]] <<- list()
    input_data_list[[key]][[current_idx]] <<- data
  }

  # 3a. TIME
  time_data <- data.frame(
    StartTimeYear      = as.integer(input$wiz_start_year),
    StartTimeMonth     = as.integer(input$wiz_start_month),
    StartTimeDay       = as.integer(input$wiz_start_day),
    StartTime          = input$wiz_start_time,
    StartTimeStandard  = input$wiz_timezone,
    RelativeStartTime  = input$wiz_relative_start,
    TimeStep           = input$wiz_timestep,
    Duration           = input$wiz_duration,
    ActivityTransition = input$wiz_activity_transition,
    stringsAsFactors=FALSE)
  time_file <- inp_path("Time.csv")
  write_csv_with_units(time_data, time_file,
    c("Gregorian","1 to 12","1 to 31","hour:min:sec",
      "R POSIX time zones","min","min","hours","min"))
  run_assembly_check(time_data, "Time", "Time")
  files_created$Time <- time_file
  update_input_list("Time", time_data)

  # 3b. BOX DATA
  n_stories <- input$wiz_num_stories %||% 2
  stack_c   <- input$wiz_stack_coeff %||% stack_coeff_table$two[min(n_stories,3)]
  wind_c    <- input$wiz_wind_coeff  %||% wind_coeff_table$two[ min(n_stories,3)]

  box_data <- data.frame(
    FloorSurfaceArea              = input$wiz_floor_area,
    RoomHeight                    = input$wiz_room_height,
    AspectRatio                   = input$wiz_aspect_ratio,
    OrientationWiderSide          = input$wiz_orientation,
    AreaToVolume                  = input$wiz_area_to_volume,
    InfiltrationSurfaceArea       = input$wiz_infiltration_area,
    StackCoefficient              = stack_c,
    WindCoefficient               = wind_c,
    DischargeCoefficient          = input$wiz_discharge_coeff,
    GravityAccel                  = input$wiz_gravity,
    MidpointHeightofWindow        = input$wiz_midpoint_height,
    NeutralPressureLevel          = input$wiz_neutral_pressure,
    OpeningEffectivenessCoefficient = input$wiz_opening_effectiveness,
    Latitude                      = input$wiz_latitude,
    Longitude                     = input$wiz_longitude,
    Altitude                      = input$wiz_altitude,
    SurfaceAlbedo                 = input$wiz_surface_albedo,
    CloudOpticalDepth             = input$wiz_cloud_optical_depth,
    CloudBase                     = input$wiz_cloud_base,
    CloudTop                      = input$wiz_cloud_top,
    IndoorReflectance             = input$wiz_indoor_reflectance,
    stringsAsFactors=FALSE)
  box_file <- inp_path("BoxData.csv")
  write.csv(box_data, box_file, row.names=FALSE)
  run_assembly_check(box_data, "BoxData", "Box Data")
  files_created$BoxData <- box_file
  update_input_list("BoxData", box_data)

  # 3c. WINDOWS
  num_windows <- input$wiz_num_windows
  if (!is.null(num_windows) && num_windows > 0) {
    windows_data <- do.call(rbind, lapply(1:num_windows, function(i) {
      data.frame(
        WindowNumber         = as.integer(i),
        Orientation          = input[[paste0("wiz_window_",i,"_orientation")]],
        AspectRatio          = input[[paste0("wiz_window_",i,"_aspect")]],
        WallSurfaceFraction  = input[[paste0("wiz_window_",i,"_fraction")]],
        GlassType            = input[[paste0("wiz_window_",i,"_glass")]],
        ObstructedAreaFraction = input[[paste0("wiz_window_",i,"_obstruction")]],
        HorizonElevationAngle  = input[[paste0("wiz_window_",i,"_horizon")]],
        stringsAsFactors=FALSE)
    }))
    windows_file <- inp_path("Windows.csv")
    write.csv(windows_data, windows_file, row.names=FALSE)
    files_created$Windows <- windows_file
    update_input_list("Windows", windows_data)
  }

  # 3d. ARTIFICIAL LIGHT LIST + SCHEDULE
  lights_mode <- input$wiz_lights_mode %||% "Use default files"

  if (lights_mode == "Upload custom files") {
    # ── Uploaded files ────────────────────────────────────────────────────
    list_upload  <- input$wiz_lights_list_file
    sched_upload <- input$wiz_lights_sched_file

    if (!is.null(list_upload)) {
      lights_data <- tryCatch(read.csv(list_upload$datapath, stringsAsFactors=FALSE, comment.char="#"),
                              error=function(e) NULL)
      if (!is.null(lights_data)) {
        lights_file <- inp_path("ArtificialLightList.csv")
        write.csv(lights_data, lights_file, row.names=FALSE)
        files_created$ArtificialLightList <- lights_file
        update_input_list("ArtificialLightList", lights_data)
      }
    }

    if (!is.null(sched_upload)) {
      light_schedule <- tryCatch(read.csv(sched_upload$datapath, stringsAsFactors=FALSE, comment.char="#"),
                                 error=function(e) NULL)
      if (!is.null(light_schedule)) {
        sched_file <- inp_path("ArtificialLightSchedule.csv")
        write.csv(light_schedule, sched_file, row.names=FALSE)
        files_created$ArtificialLightSchedule <- sched_file
        update_input_list("ArtificialLightSchedule", light_schedule)
      }
    }

  } else if (lights_mode == "Configure manually") {
    # ── Manual configuration ──────────────────────────────────────────────
    num_lights <- input$wiz_num_lights
    if (!is.null(num_lights) && num_lights > 0) {
      lights_data <- do.call(rbind, lapply(1:num_lights, function(i) {
        data.frame(
          LightNumber         = as.integer(i),
          Geometry            = input[[paste0("wiz_light_",i,"_geometry")]],
          Size                = input[[paste0("wiz_light_",i,"_size")]],
          Height              = input[[paste0("wiz_light_",i,"_height")]],
          DistanceShorterWall = input[[paste0("wiz_light_",i,"_dist_short")]],
          DistanceLongerWall  = input[[paste0("wiz_light_",i,"_dist_long")]],
          DirectionShorter    = input[[paste0("wiz_light_",i,"_dir_short")]],
          DirectionLonger     = input[[paste0("wiz_light_",i,"_dir_long")]],
          DirectionHeight     = input[[paste0("wiz_light_",i,"_dir_height")]],
          PowerEfficiency     = input[[paste0("wiz_light_",i,"_efficiency")]],
          Spectrum            = input[[paste0("wiz_light_",i,"_bulb")]],
          stringsAsFactors=FALSE)
      }))
      lights_file <- inp_path("ArtificialLightList.csv")
      write.csv(lights_data, lights_file, row.names=FALSE)
      files_created$ArtificialLightList <- lights_file
      update_input_list("ArtificialLightList", lights_data)

      # Use HOT-edited schedule if available, otherwise generate from defaults
      light_schedule <- if (!is.null(wizard_state$light_schedule) &&
                            nrow(wizard_state$light_schedule) > 0) {
        wizard_state$light_schedule
      } else {
        generate_light_schedule(
          data.frame(Time=numeric(0), Light=character(0), Power=numeric(0)),
          num_lights,
          (input$wiz_duration %||% 27) * 60,
          input$wiz_activity_transition %||% 0.1)
      }
      sched_file <- inp_path("ArtificialLightSchedule.csv")
      write.csv(light_schedule, sched_file, row.names=FALSE)
      files_created$ArtificialLightSchedule <- sched_file
      update_input_list("ArtificialLightSchedule", light_schedule)
    }

  } else {
    # ── Use default files (copy from Input/ into timestamped folder) ──────
    default_list_path  <- find_input_file("ArtificialLightList")
    default_sched_path <- find_input_file("ArtificialLightSchedule")

    for (pair in list(
      list(src=default_list_path,  key="ArtificialLightList",  dest="ArtificialLightList.csv"),
      list(src=default_sched_path, key="ArtificialLightSchedule", dest="ArtificialLightSchedule.csv")
    )) {
      if (!is.null(pair$src) && file.exists(pair$src)) {
        df <- tryCatch(read.csv(pair$src, stringsAsFactors=FALSE, comment.char="#"), error=function(e) NULL)
        if (!is.null(df)) {
          out_path <- inp_path(pair$dest)
          write.csv(df, out_path, row.names=FALSE)
          files_created[[pair$key]] <- out_path
          update_input_list(pair$key, df)
        }
      }
    }
  }

  # 3e. PHYSICAL ENVIRONMENT
  phys_mode <- input$wiz_phys_env_mode %||% "Use default file"

  if (phys_mode == "Manual entry") {
    # Prefer the HOT-edited schedule table if available
    phys_env <- if (!is.null(wizard_state$phys_schedule) &&
                    nrow(wizard_state$phys_schedule) > 0) {
      wizard_state$phys_schedule
    } else {
      initial_values <- list(
        Ti=input$wiz_temp_indoor_init, To=input$wiz_temp_outdoor_init,
        RH=input$wiz_rh_init, BP=input$wiz_bp_init, Wind=input$wiz_wind_init,
        OpenWindowArea=0, QBal=0, QUnbal=0, QFilter=0)
      final_values <- list(
        Ti=input$wiz_temp_indoor_final, To=input$wiz_temp_outdoor_final,
        RH=input$wiz_rh_final, BP=input$wiz_bp_final, Wind=input$wiz_wind_final,
        OpenWindowArea=0, QBal=0, QUnbal=0, QFilter=0)
      generate_physical_environment(
        data.frame(Time=numeric(0),Variable=character(0),Value=numeric(0)),
        (input$wiz_duration %||% 27)*60, initial_values, final_values,
        input$wiz_activity_transition %||% 0.1)
    }
    phys_file <- inp_path("PhysicalEnvironmentData.csv")
    write.csv(phys_env, phys_file, row.names=FALSE)
    run_assembly_check(phys_env, "PhysicalEnvironment", "Physical Environment",
      duration_hours = input$wiz_duration %||% 27)
    files_created$PhysicalEnvironment <- phys_file
    update_input_list("PhysicalEnvironment", phys_env)

  } else if (phys_mode == "Upload custom file") {
    req_file <- input$wiz_phys_env_file
    if (!is.null(req_file)) {
      df <- tryCatch(read.csv(req_file$datapath, stringsAsFactors=FALSE, comment.char="#"), error=function(e) NULL)
      if (!is.null(df)) {
        dest <- inp_path(req_file$name)
        file.copy(req_file$datapath, dest, overwrite=TRUE)
        files_created$PhysicalEnvironment <- dest
        update_input_list("PhysicalEnvironment", df)
      }
    }
  }
  # "Use default file" — already seeded from file_paths_advanced in step 1

  # 3f. ACTIVITIES
  act_mode <- input$wiz_act_mode %||% "Use default file"

  if (act_mode == "Manual entry") {
    all_acts  <- c("Generic","Adult","Smoking","GasCooking.Persily1998","Incense.Manoukian2013")
    dur_min   <- (input$wiz_duration %||% 27) * 60
    all_times <- sort(unique(c(0, dur_min, unlist(lapply(all_acts, function(a) {
      s <- wizard_state$activity_schedules[[a]]
      if (!is.null(s) && nrow(s) > 0) s$Time else numeric(0)
    })))))
    acts_wide <- data.frame(Time = all_times, stringsAsFactors = FALSE)
    for (act_col in all_acts) {
      s <- wizard_state$activity_schedules[[act_col]]
      if (!is.null(s) && nrow(s) > 0) {
        s <- s[order(s$Time), ]
        acts_wide[[act_col]] <- sapply(all_times, function(t) {
          before <- s[s$Time <= t, ]
          if (nrow(before) == 0) s$Value[1] else tail(before$Value, 1)
        })
      } else {
        acts_wide[[act_col]] <- 0
      }
    }
    act_file <- inp_path("Activities.csv")
    write.csv(acts_wide, act_file, row.names=FALSE)
    run_assembly_check(acts_wide, "Activities", "Activities",
      duration_hours = input$wiz_duration %||% 27)
    files_created$Activities <- act_file
    update_input_list("Activities", acts_wide)

  } else if (act_mode == "Upload custom file") {
    req_file <- input$wiz_act_file
    if (!is.null(req_file)) {
      df <- tryCatch(read.csv(req_file$datapath, stringsAsFactors=FALSE, comment.char="#"), error=function(e) NULL)
      if (!is.null(df)) {
        dest <- inp_path(req_file$name)
        file.copy(req_file$datapath, dest, overwrite=TRUE)
        files_created$Activities <- dest
        update_input_list("Activities", df)
      }
    }
  }
  # "Use default file" — seeded from file_paths_advanced in step 1

  # 3g. OUTDOOR CONCENTRATIONS
  oc_mode <- input$wiz_outdoor_conc_mode %||% "Use default file"

  if (oc_mode == "Manual entry" && !is.null(wizard_state$outdoor_manual)) {
    oc_file <- inp_path("OutdoorConcentrations.csv")
    write.csv(wizard_state$outdoor_manual, oc_file, row.names=FALSE)
    run_assembly_check(wizard_state$outdoor_manual, "OutdoorConcentrations",
      "Outdoor Concentrations", duration_hours = input$wiz_duration %||% 27)
    files_created$OutdoorConcentrations <- oc_file
    update_input_list("OutdoorConcentrations", wizard_state$outdoor_manual)

  } else if (oc_mode == "Upload custom file") {
    req_file <- input$wiz_outdoor_conc_file
    if (!is.null(req_file)) {
      df <- tryCatch(read.csv(req_file$datapath, stringsAsFactors=FALSE, comment.char="#"), error=function(e) NULL)
      if (!is.null(df)) {
        dest <- inp_path(req_file$name)
        file.copy(req_file$datapath, dest, overwrite=TRUE)
        files_created$OutdoorConcentrations <- dest
        update_input_list("OutdoorConcentrations", df)
      }
    }
  }
  # "Use default file" — seeded in step 1

  # 3h. Copy default files to the snapshot folder (suffixed by instance).
  #     Covers both the unconditional statics and the "Use default file/files"
  #     modes for PhysicalEnvironment, Activities, OutdoorConcentrations,
  #     and all light-related files.

  # Always-copied statics
  static_defaults <- c("GlassTransmission.csv", "EmissionProfiles.csv",
                        "Vd&P-Carslaw 2012.csv", "InitialIndoorConcentrations.csv",
                        "ArtificialLightSpectra.csv")
  for (f in static_defaults) {
    src <- find_input_file(tools::file_path_sans_ext(f))
    if (is.null(src)) src <- file.path("Input", f)
    if (!is.null(src) && file.exists(src)) file.copy(src, inp_path(f), overwrite=TRUE)
  }

  # Physical Environment — copy default when not manually entered / uploaded
  if ((input$wiz_phys_env_mode %||% "Use default file") == "Use default file") {
    src <- file_paths_advanced[3]   # PhysicalEnvironmentData
    if (!is.null(src) && file.exists(src)) {
      df <- tryCatch(read.csv(src, stringsAsFactors=FALSE, comment.char="#"), error=function(e) NULL)
      if (!is.null(df)) {
        dest <- inp_path(basename(src))
        write.csv(df, dest, row.names=FALSE)
        files_created$PhysicalEnvironment <- dest
        update_input_list("PhysicalEnvironment", df)
      }
    }
  }

  # Activities — copy default when not manually entered / uploaded
  if ((input$wiz_act_mode %||% "Use default file") == "Use default file") {
    src <- file_paths_advanced[5]   # Activities
    if (!is.null(src) && file.exists(src)) {
      df <- tryCatch(read.csv(src, stringsAsFactors=FALSE, comment.char="#"), error=function(e) NULL)
      if (!is.null(df)) {
        dest <- inp_path(basename(src))
        write.csv(df, dest, row.names=FALSE)
        files_created$Activities <- dest
        update_input_list("Activities", df)
      }
    }
  }

  # Outdoor Concentrations — copy default when not manually entered / uploaded
  if ((input$wiz_outdoor_conc_mode %||% "Use default file") == "Use default file") {
    src <- file_paths_advanced[7]   # Outdoor Concentrations
    if (!is.null(src) && file.exists(src)) {
      df <- tryCatch(read.csv(src, stringsAsFactors=FALSE, comment.char="#"), error=function(e) NULL)
      if (!is.null(df)) {
        dest <- inp_path(basename(src))
        write.csv(df, dest, row.names=FALSE)
        files_created$OutdoorConcentrations <- dest
        update_input_list("OutdoorConcentrations", df)
      }
    }
  }

  # Light files — copy defaults when using default files mode
  if ((input$wiz_lights_mode %||% "Use default files") == "Use default files") {
    light_defaults <- list(
      list(idx=9,  key="IndoorLight"),
      list(idx=10, key="OutdoorLightDirect"),
      list(idx=11, key="OutdoorLightDiffuse"),
      list(idx=14, key="ArtificialLight")
    )
    for (ld in light_defaults) {
      src <- file_paths_advanced[ld$idx]
      if (!is.null(src) && src != "none" && file.exists(src)) {
        ext  <- tools::file_ext(src)
        if (tolower(ext) == "xlsx") {
          dest <- inp_path(basename(src))
          file.copy(src, dest, overwrite=TRUE)
          files_created[[ld$key]] <- dest
        } else {
          df <- tryCatch(read.csv(src, stringsAsFactors=FALSE, comment.char="#"), error=function(e) NULL)
          if (!is.null(df)) {
            dest <- inp_path(basename(src))
            write.csv(df, dest, row.names=FALSE)
            files_created[[ld$key]] <- dest
            update_input_list(ld$key, df)
          }
        }
      }
    }
  }

  # Finalize
  assign("input_data_list", input_data_list, envir=.GlobalEnv)
  cat("=== DATA ASSEMBLY COMPLETE ===\n")
  cat("OutputList populated with:", length(OutputList), "entries\n")
  cat("input_data_list keys:", length(input_data_list), "file types\n")

  list(files_created=files_created, output_dir=output_dir,
       input_data_list=input_data_list,
       assembly_errors=assembly_errors, assembly_warnings=assembly_warnings)
}