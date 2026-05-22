# shared.R
`%||%` <- function(a, b) if (is.null(a)) b else a
# =======================================================================
# Shared libraries
# =======================================================================
library(shiny)
if (!requireNamespace("rhandsontable", quietly = TRUE)) stop("Package 'rhandsontable' is required.", call. = FALSE)
library(rhandsontable)
library(shinyjs)
library(shinyBS)
library(readxl)

# =======================================================================
# ==== PASTE FROM ORIGINAL: VALIDATION FUNCTIONS MODULE (ENTIRE BLOCK)
# From "# VALIDATION FUNCTIONS MODULE" down to (but not including) "# UI MODULES"
# =======================================================================

# ===============================================================================
# VALIDATION FUNCTIONS MODULE
# ===============================================================================

# Validation result structure
create_validation_result <- function(valid = TRUE, errors = character(0), warnings = character(0)) {
  list(
    valid = valid && length(errors) == 0,
    errors = errors,
    warnings = warnings
  )
}

# Format validation messages for display
format_validation_messages <- function(result) {
  msg <- ""
  if (length(result$errors) > 0) {
    msg <- paste0(msg, "<b style='color: red;'>ERRORS:</b><br>",
                  paste("• ", result$errors, collapse = "<br>"), "<br><br>")
  }
  if (length(result$warnings) > 0) {
    msg <- paste0(msg, "<b style='color: orange;'>WARNINGS:</b><br>",
                  paste("• ", result$warnings, collapse = "<br>"))
  }
  return(msg)
}

# Generic validation: Check for required columns
validate_required_columns <- function(df, required_cols, file_type) {
  errors <- character(0)
  warnings <- character(0)
  
  if (!is.data.frame(df) || nrow(df) == 0) {
    errors <- c(errors, paste(file_type, "file is empty or not a valid data frame"))
    return(create_validation_result(FALSE, errors, warnings))
  }
  
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    errors <- c(errors, paste(file_type, "missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  create_validation_result(length(errors) == 0, errors, warnings)
}

# Generic validation: Check for numeric columns
validate_numeric_columns <- function(df, numeric_cols, file_type, allow_na = TRUE) {
  errors <- character(0)
  warnings <- character(0)
  
  for (col in numeric_cols) {
    if (col %in% names(df)) {
      non_numeric <- sum(!is.na(df[[col]]) & !is.numeric(df[[col]]))
      if (non_numeric > 0) {
        errors <- c(errors, paste("Column", col, "in", file_type, "contains non-numeric values"))
      }
      
      if (!allow_na && any(is.na(df[[col]]))) {
        warnings <- c(warnings, paste("Column", col, "in", file_type, "contains NA values"))
      }
    }
  }
  
  create_validation_result(length(errors) == 0, errors, warnings)
}

# Generic validation: Check for non-negative values
validate_non_negative <- function(df, cols, file_type) {
  errors <- character(0)
  warnings <- character(0)
  
  for (col in cols) {
    if (col %in% names(df)) {
      if (is.numeric(df[[col]])) {
        neg_count <- sum(df[[col]] < 0, na.rm = TRUE)
        if (neg_count > 0) {
          errors <- c(errors, paste("Column", col, "in", file_type, "contains", neg_count, "negative value(s)"))
        }
      }
    }
  }
  
  create_validation_result(length(errors) == 0, errors, warnings)
}

# Strip metadata/units rows that appear before real data.
# Many SIACS input files include a row like "Uncertainty,0.1,0.1,..." or
# a units row immediately after the header. These rows have a non-numeric
# value in the Time column and must be removed before any numeric checks.
strip_metadata_rows <- function(df) {
  if (!"Time" %in% names(df) || nrow(df) == 0) return(df)
  time_numeric <- suppressWarnings(as.numeric(as.character(df$Time)))
  df <- df[!is.na(time_numeric), , drop = FALSE]
  df$Time <- time_numeric[!is.na(time_numeric)]
  df
}

# Specific validation: Time data
validate_time_data <- function(df) {
  errors <- character(0)
  warnings <- character(0)
  
  required_cols <- c("StartTimeYear", "StartTimeMonth", "StartTimeDay", "StartTime", 
                     "StartTimeStandard", "RelativeStartTime", "TimeStep", "Duration")
  
  result <- validate_required_columns(df, required_cols, "Time")
  errors <- c(errors, result$errors)
  warnings <- c(warnings, result$warnings)
  
  if (result$valid) {
    # Validate year range
    if ("StartTimeYear" %in% names(df)) {
      if (df$StartTimeYear < 1900 || df$StartTimeYear > 2100) {
        warnings <- c(warnings, "StartTimeYear outside typical range (1900-2100)")
      }
      if (df$StartTimeYear < 0) {
        errors <- c(errors, "StartTimeYear cannot be negative")
      }
    }
    
    # Validate month
    if ("StartTimeMonth" %in% names(df)) {
      if (df$StartTimeMonth < 1 || df$StartTimeMonth > 12) {
        errors <- c(errors, "StartTimeMonth must be between 1 and 12")
      }
    }
    
    # Validate day
    if ("StartTimeDay" %in% names(df)) {
      if (df$StartTimeDay < 1 || df$StartTimeDay > 31) {
        errors <- c(errors, "StartTimeDay must be between 1 and 31")
      }
    }
    
    # Validate StartTime (should be in HH:MM format or numeric hours)
    if ("StartTime" %in% names(df)) {
      start_time_val <- df$StartTime
      # If it's numeric, check it's in valid range (0-24)
      if (is.numeric(start_time_val)) {
        if (start_time_val < 0 || start_time_val > 24) {
          errors <- c(errors, "StartTime must be between 0 and 24 hours")
        }
      } else if (is.character(start_time_val)) {
        # If it's character, check it matches HH:MM format
        if (!grepl("^([0-1]?[0-9]|2[0-3]):[0-5][0-9](:[0-5][0-9])?$", start_time_val)) {
          warnings <- c(warnings, "StartTime should be in HH:MM or HH:MM:SS format (e.g., '09:00' or '14:30:00')")
        }
      }
    }
    
    # Validate RelativeStartTime
    if ("RelativeStartTime" %in% names(df)) {
      if (df$RelativeStartTime < 0) {
        errors <- c(errors, "RelativeStartTime cannot be negative")
      }
    }
    
    # Validate TimeStep
    if ("TimeStep" %in% names(df)) {
      if (df$TimeStep <= 0) {
        errors <- c(errors, "TimeStep must be positive")
      }
      if (df$TimeStep > 60) {
        warnings <- c(warnings, "TimeStep greater than 60 minutes may be too coarse")
      }
    }
    
    # Validate Duration
    if ("Duration" %in% names(df)) {
      if (df$Duration <= 0) {
        errors <- c(errors, "Duration must be positive")
      }
      if (df$Duration > 8760) {
        warnings <- c(warnings, "Duration exceeds 1 year (8760 hours)")
      }
    }
  }
  
  create_validation_result(length(errors) == 0, errors, warnings)
}

# Specific validation: Box Data
validate_box_data <- function(df) {
  errors <- character(0)
  warnings <- character(0)
  
  required_cols <- c("FloorSurfaceArea", "RoomHeight")
  
  result <- validate_required_columns(df, required_cols, "Box Data")
  errors <- c(errors, result$errors)
  
  if (result$valid) {
    # Check non-negative physical dimensions
    physical_cols <- intersect(c("FloorSurfaceArea", "RoomHeight"), names(df))
    result_neg <- validate_non_negative(df, physical_cols, "Box Data")
    errors <- c(errors, result_neg$errors)
    
    # Check for zero dimensions
    for (col in physical_cols) {
      if (col %in% names(df) && any(df[[col]] == 0, na.rm = TRUE)) {
        warnings <- c(warnings, paste(col, "contains zero values - check if intentional"))
      }
    }
    
    # Validate aspect ratio if present
    if ("AspectRatio" %in% names(df)) {
      if (any(df$AspectRatio <= 0, na.rm = TRUE)) {
        errors <- c(errors, "AspectRatio must be positive")
      }
    }
    
    # Validate orientation if present (should be 0-360 degrees)
    if ("OrientationWiderSide" %in% names(df)) {
      if (any(df$OrientationWiderSide < 0 | df$OrientationWiderSide > 360, na.rm = TRUE)) {
        errors <- c(errors, "OrientationWiderSide must be between 0 and 360 degrees")
      }
    }
  }
  
  create_validation_result(length(errors) == 0, errors, warnings)
}

# Specific validation: Physical Environment
validate_physical_environment <- function(df, duration_hours = NULL) {
  errors <- character(0)
  warnings <- character(0)
  
  df <- strip_metadata_rows(df)   # remove Uncertainty / units rows before any checks
  
  required_cols <- c("Time", "Ti", "To", "RH", "BP")
  
  result <- validate_required_columns(df, required_cols, "Physical Environment")
  errors <- c(errors, result$errors)
  
  if (result$valid) {
    # Validate Time column - convert to numeric if needed
    if ("Time" %in% names(df)) {
      if (!is.numeric(df$Time)) {
        # Try to convert
        time_numeric <- suppressWarnings(as.numeric(as.character(df$Time)))
        if (all(is.na(time_numeric))) {
          errors <- c(errors, "Time column cannot be converted to numeric")
        } else {
          df$Time <- time_numeric  # silent conversion — character numerics are normal in CSV files
        }
      }
      
      if (is.numeric(df$Time)) {
        if (any(diff(df$Time) <= 0, na.rm = TRUE)) {
          errors <- c(errors, "Time values must be strictly increasing")
        }
        
        # Check duration consistency
        if (!is.null(duration_hours)) {
          max_time_minutes <- max(df$Time, na.rm = TRUE)
          expected_minutes <- duration_hours * 60
          if (max_time_minutes < expected_minutes) {
            warnings <- c(warnings, paste("Physical Environment data ends at", max_time_minutes, 
                                          "minutes but simulation duration is", expected_minutes, "minutes"))
          }
        }
      }
    }
    
    # Validate temperature ranges in Kelvin (Ti = Indoor Temp, To = Outdoor Temp)
    if ("Ti" %in% names(df)) {
      if (any(df$Ti < 223 | df$Ti > 333, na.rm = TRUE)) {
        warnings <- c(warnings, "Ti (Indoor Temperature) outside typical range (223–333 K, i.e. -50 to 60°C)")
      }
    }
    
    if ("To" %in% names(df)) {
      if (any(df$To < 213 | df$To > 333, na.rm = TRUE)) {
        warnings <- c(warnings, "To (Outdoor Temperature) outside typical range (213–333 K, i.e. -60 to 60°C)")
      }
    }
    
    # Validate relative humidity (0-1 fraction as used by the model)
    if ("RH" %in% names(df)) {
      if (any(df$RH < 0 | df$RH > 1, na.rm = TRUE)) {
        errors <- c(errors, "RH (Relative Humidity) must be between 0 and 1 (fractional, not percent)")
      }
    }
    
    # Validate barometric pressure in Pa (typical range: 80000-110000 Pa)
    if ("BP" %in% names(df)) {
      if (any(df$BP < 80000 | df$BP > 110000, na.rm = TRUE)) {
        warnings <- c(warnings, "BP (Barometric Pressure) outside typical range (80000–110000 Pa)")
      }
    }
    
    # Validate wind speed (must be non-negative)
    if ("Wind" %in% names(df)) {
      if (any(df$Wind < 0, na.rm = TRUE)) {
        errors <- c(errors, "Wind speed cannot be negative")
      }
      if (any(df$Wind > 50, na.rm = TRUE)) {
        warnings <- c(warnings, "Wind speed exceeds 50 m/s (hurricane force winds)")
      }
    }
    
    # Validate ventilation rates (if present)
    if ("QBal" %in% names(df)) {
      if (any(df$QBal < 0, na.rm = TRUE)) {
        errors <- c(errors, "QBal (Balanced Ventilation Rate) cannot be negative")
      }
    }
    
    if ("QUnbal" %in% names(df)) {
      if (any(df$QUnbal < 0, na.rm = TRUE)) {
        errors <- c(errors, "QUnbal (Unbalanced Ventilation Rate) cannot be negative")
      }
    }
    
    if ("QFilter" %in% names(df)) {
      if (any(df$QFilter < 0, na.rm = TRUE)) {
        errors <- c(errors, "QFilter (Filter Ventilation Rate) cannot be negative")
      }
    }
    
    if ("OpenWindowArea" %in% names(df)) {
      if (any(df$OpenWindowArea < 0, na.rm = TRUE)) {
        errors <- c(errors, "OpenWindowArea cannot be negative")
      }
    }
  }
  
  create_validation_result(length(errors) == 0, errors, warnings)
}

# Specific validation: Outdoor Concentrations
validate_outdoor_concentrations <- function(df, duration_hours = NULL) {
  errors <- character(0)
  warnings <- character(0)
  
  df <- strip_metadata_rows(df)   # remove Uncertainty / units rows before any checks
  
  if (!"Time" %in% names(df)) {
    errors <- c(errors, "Outdoor Concentrations must have a Time column")
    return(create_validation_result(FALSE, errors, warnings))
  }
  
  # Validate Time column - try to convert to numeric if it's character
  if (!is.numeric(df$Time)) {
    # Try to convert
    time_numeric <- suppressWarnings(as.numeric(as.character(df$Time)))
    if (all(is.na(time_numeric))) {
      errors <- c(errors, "Time column cannot be converted to numeric")
    } else {
      df$Time <- time_numeric  # silent conversion — character numerics are normal in CSV files
    }
  }
  
  if (is.numeric(df$Time)) {
    if (any(diff(df$Time) <= 0, na.rm = TRUE)) {
      errors <- c(errors, "Time values must be strictly increasing")
    }
    
    # Check duration consistency - make this a warning instead of error
    if (!is.null(duration_hours)) {
      max_time_minutes <- max(df$Time, na.rm = TRUE)
      expected_minutes <- duration_hours * 60
      if (max_time_minutes < expected_minutes) {
        warnings <- c(warnings, paste("Outdoor Concentrations data ends at", max_time_minutes, 
                                      "minutes but simulation duration is", expected_minutes, 
                                      "minutes. Last values will be used for remaining time."))
      }
    }
  }
  
  # Check for negative concentrations in all species columns (excluding Time)
  concentration_cols <- setdiff(names(df), "Time")
  for (col in concentration_cols) {
    if (is.numeric(df[[col]])) {
      if (any(df[[col]] < 0, na.rm = TRUE)) {
        errors <- c(errors, paste("Species", col, "has negative concentration values"))
      }
    }
  }
  
  create_validation_result(length(errors) == 0, errors, warnings)
}

# Validation for light-related files (less strict - mainly structural checks)
validate_light_file <- function(df, file_type) {
  errors <- character(0)
  warnings <- character(0)
  
  df <- strip_metadata_rows(df)   # remove units / comment rows before any checks
  
  if (!"Time" %in% names(df) && file_type != "GlassTransmission" && file_type != "ArtificialLightList" && file_type != "ArtificialLightSpectra") {
    warnings <- c(warnings, paste(file_type, "should typically have a Time column"))
  }
  
  # Check for negative values — skip Direction columns (legitimately range -1 to 1)
  direction_cols <- c("DirectionShorter", "DirectionLonger", "DirectionHeight")
  for (col in names(df)) {
    if (is.numeric(df[[col]]) && col != "Time" && col != "SolarElevationAngle" &&
        !grepl("^x\\d+", col) && !col %in% direction_cols) {
      if (any(df[[col]] < 0, na.rm = TRUE)) {
        neg_count <- sum(df[[col]] < 0, na.rm = TRUE)
        warnings <- c(warnings, paste("Column", col, "contains", neg_count, "negative values"))
      }
    }
  }
  
  create_validation_result(length(errors) == 0, errors, warnings)
}

# Specific validation: Emission Profiles
validate_emission_profiles <- function(df) {
  errors <- character(0)
  warnings <- character(0)
  
  if (!"ProfileName" %in% names(df)) {
    errors <- c(errors, "Emission Profiles must have a ProfileName column")
    return(create_validation_result(FALSE, errors, warnings))
  }
  
  # Check for duplicate profile names
  if (any(duplicated(df$ProfileName))) {
    errors <- c(errors, "Duplicate ProfileName entries found")
  }
  
  # Check for negative emission rates
  emission_cols <- setdiff(names(df), "ProfileName")
  for (col in emission_cols) {
    if (is.numeric(df[[col]])) {
      if (any(df[[col]] < 0, na.rm = TRUE)) {
        errors <- c(errors, paste("Emission profile for", col, "contains negative values"))
      }
    }
  }
  
  create_validation_result(length(errors) == 0, errors, warnings)
}

# Specific validation: Activities
validate_activities <- function(df, duration_hours = NULL) {
  errors <- character(0)
  warnings <- character(0)
  
  df <- strip_metadata_rows(df)   # remove Uncertainty / units rows before any checks
  
  if (!"Time" %in% names(df)) {
    errors <- c(errors, "Activities must have a Time column")
    return(create_validation_result(FALSE, errors, warnings))
  }
  
  # Validate Time column
  if (!is.numeric(df$Time)) {
    errors <- c(errors, "Time column must be numeric")
  } else {
    if (any(diff(df$Time) <= 0, na.rm = TRUE)) {
      errors <- c(errors, "Time values must be strictly increasing")
    }
    
    # Check duration consistency - make this a warning instead of error
    if (!is.null(duration_hours)) {
      max_time_minutes <- max(df$Time, na.rm = TRUE)
      expected_minutes <- duration_hours * 60
      if (max_time_minutes < expected_minutes) {
        warnings <- c(warnings, paste("Activities data ends at", max_time_minutes, 
                                      "minutes but simulation duration is", expected_minutes, 
                                      "minutes. Activities will be held constant after the last time point."))
      }
    }
  }
  
  # Check for negative activity levels
  activity_cols <- setdiff(names(df), "Time")
  for (col in activity_cols) {
    if (is.numeric(df[[col]])) {
      if (any(df[[col]] < 0, na.rm = TRUE)) {
        warnings <- c(warnings, paste("Activity", col, "contains negative values"))
      }
    }
  }
  
  create_validation_result(length(errors) == 0, errors, warnings)
}

# Specific validation: Deposition Velocity
validate_deposition_velocity <- function(df) {
  errors <- character(0)
  warnings <- character(0)
  
  required_cols <- c("spcname", "dvel")
  
  result <- validate_required_columns(df, required_cols, "Deposition Velocity")
  errors <- c(errors, result$errors)
  
  if (result$valid) {
    # Check for negative deposition velocities
    if ("dvel" %in% names(df)) {
      if (any(df$dvel < 0, na.rm = TRUE)) {
        errors <- c(errors, "dvel (Deposition Velocity) cannot be negative")
      }
    }
    
    # Check penetration factor (0-1)
    if ("PFactor" %in% names(df)) {
      if (any(df$PFactor < 0 | df$PFactor > 1, na.rm = TRUE)) {
        errors <- c(errors, "PFactor (Penetration Factor) must be between 0 and 1")
      }
    }
    
    # Check filter efficiency (0-1)
    if ("FilterEfficiency" %in% names(df)) {
      if (any(df$FilterEfficiency < 0 | df$FilterEfficiency > 1, na.rm = TRUE)) {
        errors <- c(errors, "FilterEfficiency must be between 0 and 1")
      }
    }
    
    if ("IntakeFilterEfficiency" %in% names(df)) {
      if (any(df$IntakeFilterEfficiency < 0 | df$IntakeFilterEfficiency > 1, na.rm = TRUE)) {
        errors <- c(errors, "IntakeFilterEfficiency must be between 0 and 1")
      }
    }
  }
  
  create_validation_result(length(errors) == 0, errors, warnings)
}

# Specific validation: Initial Indoor Concentrations
validate_initial_concentrations <- function(df) {
  errors <- character(0)
  warnings <- character(0)
  
  # Check for negative concentrations
  for (col in names(df)) {
    if (is.numeric(df[[col]])) {
      if (any(df[[col]] < 0, na.rm = TRUE)) {
        errors <- c(errors, paste("Species", col, "has negative initial concentration"))
      }
    }
  }
  
  create_validation_result(length(errors) == 0, errors, warnings)
}

# Specific validation: Windows data
validate_windows <- function(df) {
  errors <- character(0)
  warnings <- character(0)
  
  required_cols <- c("WindowNumber", "Orientation")
  
  result <- validate_required_columns(df, required_cols, "Windows")
  errors <- c(errors, result$errors)
  
  if (result$valid) {
    # Check orientation range (0-360)
    if ("Orientation" %in% names(df)) {
      if (any(df$Orientation < 0 | df$Orientation > 360, na.rm = TRUE)) {
        errors <- c(errors, "Orientation must be between 0 and 360 degrees")
      }
    }
    
    # Check wall surface fraction (0-1)
    if ("WallSurfaceFraction" %in% names(df)) {
      if (any(df$WallSurfaceFraction < 0 | df$WallSurfaceFraction > 1, na.rm = TRUE)) {
        errors <- c(errors, "WallSurfaceFraction must be between 0 and 1")
      }
    }
    
    # Check obstructed area fraction (0-1)
    if ("ObstructedAreaFraction" %in% names(df)) {
      if (any(df$ObstructedAreaFraction < 0 | df$ObstructedAreaFraction > 1, na.rm = TRUE)) {
        errors <- c(errors, "ObstructedAreaFraction must be between 0 and 1")
      }
    }
  }
  
  create_validation_result(length(errors) == 0, errors, warnings)
}

# Master validation dispatcher
validate_data_file <- function(df, file_type, duration_hours = NULL) {
  result <- switch(file_type,
                   "Time" = validate_time_data(df),
                   "BoxData" = validate_box_data(df),
                   "PhysicalEnvironment" = validate_physical_environment(df, duration_hours),
                   "OutdoorConcentrations" = validate_outdoor_concentrations(df, duration_hours),
                   "EmissionProfiles" = validate_emission_profiles(df),
                   "Activities" = validate_activities(df, duration_hours),
                   "DepositionVelocity" = validate_deposition_velocity(df),
                   "InitialValues" = validate_initial_concentrations(df),
                   "Windows" = validate_windows(df),
                   "IndoorLight" = validate_light_file(df, "IndoorLight"),
                   "OutdoorLightDirect" = validate_light_file(df, "OutdoorLightDirect"),
                   "OutdoorLightDiffuse" = validate_light_file(df, "OutdoorLightDiffuse"),
                   "GlassTransmission" = validate_light_file(df, "GlassTransmission"),
                   "ArtificialLight" = validate_light_file(df, "ArtificialLight"),
                   "ArtificialLightList" = validate_light_file(df, "ArtificialLightList"),
                   "ArtificialLightSpectra" = validate_light_file(df, "ArtificialLightSpectra"),
                   "ArtificialLightSchedule" = validate_light_file(df, "ArtificialLightSchedule"),
                   create_validation_result(TRUE, character(0), "File type validated with basic checks")
  )
  
  return(result)
}

# =======================================================================
output_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Enter Output Settings"),
    helpText("Edit the values in the table below or leave defaults."),
    rHandsontableOutput(ns("inputTable")),
    actionButton(ns("saveCloseBtn"), "Save output filenames")
  )
}

output_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    default_values <- c(
      "OutputTable" = "./Output/ATL-CMAQ for Ambient",
      "OutputBasicChart" = "./Output/ATL-CMAQ for Ambient",
      "OutputTimeDerivatives" = "./Output/ATL-CMAQ for Ambient Derivatives",
      "OutputMassBalanceComponents" = "./Output/ATL-CMAQ for Ambient Mass Balance",
      "OutputSensitivity" = "./Output/ATL-CMAQ for Ambient Sensitivity",
      "OutputUncertainty" = "./Output/ATL-CMAQ for Ambient uncertainty"
    )
    initial_data <- data.frame(Item = names(default_values), Value = unname(default_values), stringsAsFactors = FALSE)
    values <- reactiveVal(initial_data)
    
    output$inputTable <- renderRHandsontable({
      rhandsontable(values(), rowHeaders = NULL)
    })
    
    observeEvent(input$saveCloseBtn, {
      if (!is.null(input$inputTable)) {
        df <- hot_to_r(input$inputTable)
        values(df)
        final_list <- setNames(as.list(df$Value), df$Item)
        
        # Save immediately to global environment
        if (!exists("OutputList", envir = .GlobalEnv)) {
          assign("OutputList", list(), envir = .GlobalEnv)
        }
        current_list <- get("OutputList", envir = .GlobalEnv)
        current_list[[length(current_list) + 1]] <- final_list
        assign("OutputList", current_list, envir = .GlobalEnv)
      }
      showModal(modalDialog("Output settings saved.", easyClose = TRUE))
    })
  })
}

render_input_block <- function(i) {
  tagList(
    bsTooltip(id = paste0("title_", i), title = tooltips_advanced[i], placement = "right", trigger = "hover"),
    div(id = paste0("title_", i),
        selectInput(paste0("data_source", i), titles_advanced[i],
                    choices = c("Preload data", "Upload a file", "Import from online sources", "Create a file"))
    ),
    conditionalPanel(
      condition = sprintf("input.data_source%d=='%s'", i, "Preload data"),
      actionButton(paste0("preload_table", i), paste("Preload", titles_advanced[i])),
      actionButton(paste0("preview_preload", i), paste("Preview", titles_advanced[i]))
    ),
    conditionalPanel(
      condition = sprintf("input.data_source%d=='%s'", i, "Upload a file"),
      fileInput(paste0("file", i), titles_advanced[i], accept = c(".csv", ".xlsx")),
      actionButton(paste0("show_table", i), paste("Preview", titles_advanced[i])),
      actionButton(paste0("validate", i), paste("Validate", titles_advanced[i]), 
                   style = "background-color: #28a745; color: white;")
    ),
    conditionalPanel(
      condition = sprintf("input.data_source%d=='%s'", i, "Import from online sources"),
      textInput(paste0("url", i), paste("Enter the URL for", titles_advanced[i])),
      actionButton(paste0("import_table", i), paste("Import", titles_advanced[i])),
      actionButton(paste0("preview_import", i), paste("Preview", titles_advanced[i])),
      actionButton(paste0("validate_import", i), paste("Validate", titles_advanced[i]),
                   style = "background-color: #28a745; color: white;")
    ),
    conditionalPanel(
      condition = sprintf("input.data_source%d=='%s'", i, "Create a file"),
      actionButton(paste0("create_table", i), paste("Create", titles_advanced[i])),
      actionButton(paste0("preview_create", i), paste("Preview", titles_advanced[i]))
    ),
    # Validation status indicator
    uiOutput(paste0("validation_status_", i))
  )
}

# Functions -----
# custom modal
modal_ui <- function(id, title, ..., size = "m", footer = modalButton("Close"), static = FALSE) {
  div(
    class = "modal fade",
    id = id,
    tabindex = "-1",
    `data-backdrop` = if (static) "static" else "true",
    `data-keyboard` = if (static) "false" else "true",
    div(
      class = switch(size,
                     m = "modal-dialog",
                     l = "modal-dialog modal-lg",
                     xl = "modal-dialog modal-xl"
      ),
      div(
        class = "modal-content",
        div(
          class = "modal-header",
          title,
          modalButton(HTML("&#x2715;"))
        ),
        div(
          class = "modal-body",
          list(...)
        ),
        div(
          class = "modal-footer",
          footer
        )
      )
    )
  )
}

modal_toggle <- function(id, e = "toggle") {
  shinyjs::runjs(sprintf("$('#%s').modal('%s');", id, e))
}

modal_actionButton <- function(inputId, label, target, icon = NULL, width = NULL, disabled = F, ...) {
  actionButton(
    inputId = inputId, label = label, icon = icon, width = width, disabled = disabled,
    `data-toggle` = "modal", `data-bs-toggle` = "modal",
    `data-target` = paste0("#", target), `data-bs-target` = paste0("#", target)
  )
}

# Global Variables -----
titles_advanced <- c("Initial Values", "Deposition Velocity", "Physical Environment", "Emission Profiles", 
                     "Activities", "Box Data", "Outdoor Concentrations", "Time", "Indoor Light", 
                     "Outdoor Light Direct", "Outdoor Light Diffuse", "Windows", "Glass Transmission", 
                     "Artificial Light", "Artificial Light List", "Artificial Light Spectra", 
                     "Artificial Light Schedule")

file_paths_advanced <- c("./Input/InitialIndoorConcentrations.csv", "./Input/Vd&P-Carslaw 2012.csv", 
                         "./Input/PhysicalEnvironmentData_CMAQ Atlanta_48hrs.csv", "./Input/EmissionProfiles.csv", 
                         "./Input/Activities.csv", "./Input/BoxData - Atlanta.csv", 
                         "./Input/Outdoor Concentrations - Atlanta July CMAQ.csv", "./Input/Time.csv", 
                         "./Input/IndoorLightAtlanta.xlsx", "./Input/DirectLight-Atlanta-July7-8.csv", 
                         "./Input/DiffuseLight-Atlanta-July7-8.csv", "./Input/ThreeWindowsESW.csv", 
                         "./Input/GlassTransmission.csv", "./Input/ArtifLightAtlanta.csv", 
                         "./Input/ArtificialLightList.csv", "./Input/ArtificialLightSpectra.csv", 
                         "./Input/ArtificialLightSchedule.csv")

tooltips_advanced <- c("Initial values supplied for indoor concentrations", "Deposition velocities and parameters", 
                       "Physical environment data", "Emission profiles", "Activities data", "Box data for the model", 
                       "Outdoor concentrations data", "Time data", "Indoor light data", "Outdoor direct light data", 
                       "Outdoor diffuse light data", "Windows data", "Glass transmission data", "Artificial light data", 
                       "List of artificial lights", "Spectra of artificial lights", "Schedule of artificial lights")

# Map file indices to validation types
file_type_map <- c("InitialValues", "DepositionVelocity", "PhysicalEnvironment", "EmissionProfiles",
                   "Activities", "BoxData", "OutdoorConcentrations", "Time", "IndoorLight",
                   "OutdoorLightDirect", "OutdoorLightDiffuse", "Windows", "GlassTransmission",
                   "ArtificialLight", "ArtificialLightList", "ArtificialLightSpectra",
                   "ArtificialLightSchedule")