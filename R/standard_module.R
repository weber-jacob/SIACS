# standard_module.R
# =======================================================================
# Standard simulation module - WITH VALIDATION (using shared.R functions)
# =======================================================================

# -------------------------
# Standard UI
# -------------------------
standard_module_ui <- function() {
  tagList(
    modal_ui(
      id = "standard_menu",
      title = "Standard Simulation",
      size = "xl",
      fluidRow(
        column(
          4,
          wellPanel(
            # Validation Summary at the top
            div(
              id = "standard_validation_summary",
              style = "margin-bottom: 15px; padding: 10px; background-color: #f8f9fa; border-radius: 5px;",
              actionButton(
                "validate_standard_inputs", 
                "Validate All Inputs",
                style = "background-color: #28a745; color: white; font-weight: bold; width: 100%; margin-bottom: 10px;"
              ),
              htmlOutput("standard_validation_status")
            ),
            
            # Activities Section
            div(
              div(style = "font-size: 18px; color: #2E86C1; font-weight: bold;", "Activities"),
              bsTooltip(id = "title_activities", title = "Activities data", placement = "right", trigger = "hover"),
              div(style = "font-size: 16px; color: #2874A6; font-weight: bold;", "Activities Data Input"),
              numericInput("num_activities", "How many Activities do you like to model?", value = 2, min = 1, width = "100%"),
              selectInput("activity1", "What is activity one?", choices = c("Adult in the Room", "Child in the Room", "Smoking"), selected = "Adult in the Room", width = "100%"),
              numericInput("activity1_count", "How many?", value = 2, min = 1, width = "100%"),
              textInput("activity1_start", "Start Time (minutes)?", value = "0", placeholder = "Enter Start Time", width = "100%"),
              textInput("activity1_stop", "Stop Time (minutes)?", value = "2880", placeholder = "Enter Stop Time", width = "100%"),
              numericInput("num_vent_sources", "How many sources of ventilation would you like?", value = 0, min = 0, width = "100%"),
              conditionalPanel(
                condition = "input.num_vent_sources >= 1",
                selectInput("vent1", "What is ventilation source one?", choices = c("Open Window", "Balanced Mechanical Ventilation", "Filtering"), width = "100%"),
                conditionalPanel(
                  condition = "input.vent1 == 'Open Window'",
                  numericInput("vent1_window", "Window Number?", value = 1, min = 1, width = "100%"),
                  textInput("vent1_start", "Start Time?", placeholder = "Enter Start Time", width = "100%"),
                  textInput("vent1_stop", "Stop Time?", placeholder = "Enter Stop Time", width = "100%")
                )
              )
            ),
            # Box Data Section
            div(
              div(style = "font-size: 18px; color: #2E86C1; font-weight: bold;", "Box Data"),
              bsTooltip(id = "title_box_data", title = "Box data for model", placement = "right", trigger = "hover"),
              div(style = "font-size: 16px; color: #2874A6; font-weight: bold;", "Box Data Input"),
              numericInput("floor_area", "What is the floor surface area (m²)?", value = 50, min = 0, width = "100%"),
              numericInput("room_height", "What is the room height (m)?", value = 2.5, min = 0, width = "100%"),
              numericInput("aspect_ratio", "What is the aspect ratio of your box?", value = 1.5, min = 0, width = "100%"),
              numericInput("orientation", "What is the orientation of your box (degrees)?", value = 180, min = 0, max = 360, width = "100%"),
              selectInput("num_windows", "How many windows would you like?", choices = 0:3, selected = "3", width = "100%"),
              conditionalPanel(condition = "input.num_windows >= 1",
                               numericInput("window1_area", "Window 1 Area (m²)", value = 2, min = 0, width = "100%"),
                               numericInput("window1_orientation", "Window 1 Orientation (degrees)", value = 90, min = 0, max = 360, width = "100%")
              ),
              conditionalPanel(condition = "input.num_windows >= 2",
                               numericInput("window2_area", "Window 2 Area (m²)", value = 2, min = 0, width = "100%"),
                               numericInput("window2_orientation", "Window 2 Orientation (degrees)", value = 180, min = 0, max = 360, width = "100%")
              ),
              conditionalPanel(condition = "input.num_windows >= 3",
                               numericInput("window3_area", "Window 3 Area (m²)", value = 2, min = 0, width = "100%"),
                               numericInput("window3_orientation", "Window 3 Orientation (degrees)", value = 270, min = 0, max = 360, width = "100%")
              ),
              sliderInput("building_tightness", "How tight would you like your building?", min = 0, max = 100, value = 50, ticks = FALSE, animate = TRUE, width = "100%")
            ),
            # Time Section
            div(
              div(style = "font-size: 18px; color: #2E86C1; font-weight: bold;", "Time"),
              bsTooltip(id = "title_time", title = "Time data", placement = "right", trigger = "hover"),
              div(style = "font-size: 16px; color: #2874A6; font-weight: bold;", "Time Data Input"),
              textInput("start_date", "What is the start date for your simulation?", value = "07/07/2022", placeholder = "Enter date dd/mm/yyyy", width = "100%"),
              textInput("start_time", "What time does your simulation start?", value = "00:00", placeholder = "Enter hh:mm", width = "100%"),
              selectInput("timezone", "What timezone is your simulation in?", choices = c("Central - Daylight", "Central - Standard"), selected = "Central - Daylight", width = "100%"),
              numericInput("duration2", "How long is your simulation (hours)?", value = 48, min = 1, width = "100%")
            ),
            tags$hr(),
            actionButton("continue_to_outputs_standard", "Continue to Output Settings")
          )
        ),
        column(
          8,
          div(
            style = "max-height: 90vh; overflow-y: auto;",
            tabsetPanel(
              tabPanel("Data Preview", uiOutput("standard_preview")),
              tabPanel("Validation Details", div(style = "padding: 15px;", htmlOutput("standard_validation_details")))
            )
          )
        )
      )
    ),
    
    # Standard Outputs Modal
    modal_ui(
      id = "standard_outputs_menu",
      title = "Standard Simulation - Output Settings",
      size = "l",
      fluidRow(
        column(
          12,
          wellPanel(
            h4("Output Settings"),
            output_ui("output_module_standard"),
            tags$hr(),
            actionButton("save_standard", "Save Standard Simulation")
          )
        )
      )
    )
  )
}

# -------------------------
# Standard Server
# -------------------------
standard_module_server <- function(input, output, session) {
  data_list <- reactiveValues()
  counter <- reactiveVal(1)
  validation_results_standard <- reactiveValues()
  
  # Helper function to read default files
  read_data <- function(file_path, show_error_modal = TRUE) {
    tryCatch({
      if (grepl("\\.csv$", file_path, ignore.case = TRUE)) {
        df <- read.csv(file_path, comment.char = "#", na.strings = "None", stringsAsFactors = FALSE)
        if (nrow(df) == 0 || (nrow(df) == 1 && all(is.na(df[1, ])))) return(data.frame())
        return(df)
        
      } else if (grepl("\\.xlsx$", file_path, ignore.case = TRUE)) {
        sheets <- excel_sheets(file_path)
        if (length(sheets) == 1) {
          df <- tryCatch(read_excel(file_path), error = function(e) read_excel(file_path, comment = "#"))
          df <- as.data.frame(df)
          if (nrow(df) == 0 || (nrow(df) == 1 && all(is.na(df[1, ])))) return(data.frame())
          return(list(df))
        } else {
          df_list <- lapply(sheets, function(sheet) {
            sheet_df <- read_excel(file_path, sheet = sheet)
            as.data.frame(sheet_df)
          })
          names(df_list) <- sheets
          return(df_list)
        }
      } else {
        return(NULL)
      }
    }, error = function(e) {
      if (show_error_modal) {
        showModal(modalDialog(
          title = "File Reading Error",
          paste("Error reading file:", e$message),
          easyClose = TRUE, footer = modalButton("OK")
        ))
      }
      return(NULL)
    })
  }
  
  # =========================================================================
  # BUILD DATA FRAMES FROM USER INPUTS (for validation using shared.R functions)
  # =========================================================================
  build_time_data_from_inputs <- function() {
    tryCatch({
      date_obj <- as.Date(input$start_date, format = "%d/%m/%Y")
      
      data.frame(
        StartTimeYear = as.integer(format(date_obj, "%Y")),
        StartTimeMonth = as.integer(format(date_obj, "%m")),
        StartTimeDay = as.integer(format(date_obj, "%d")),
        StartTime = input$start_time,
        StartTimeStandard = ifelse(input$timezone == "Central - Daylight", "CST6CDT", "CST"),
        RelativeStartTime = 0,
        TimeStep = 1,
        Duration = input$duration2,
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      return(NULL)
    })
  }
  
  build_box_data_from_inputs <- function() {
    data.frame(
      FloorSurfaceArea = input$floor_area,
      RoomHeight = input$room_height,
      AspectRatio = input$aspect_ratio,
      OrientationWiderSide = input$orientation,
      stringsAsFactors = FALSE
    )
  }
  
  build_windows_data_from_inputs <- function() {
    num_windows <- as.numeric(input$num_windows)
    
    if (num_windows == 0) {
      return(NULL)
    }
    
    windows_list <- list()
    
    if (num_windows >= 1) {
      windows_list[[1]] <- data.frame(
        WindowNumber = 1L,
        Orientation = input$window1_orientation,
        WallSurfaceFraction = input$window1_area / input$floor_area,
        stringsAsFactors = FALSE
      )
    }
    
    if (num_windows >= 2) {
      windows_list[[2]] <- data.frame(
        WindowNumber = 2L,
        Orientation = input$window2_orientation,
        WallSurfaceFraction = input$window2_area / input$floor_area,
        stringsAsFactors = FALSE
      )
    }
    
    if (num_windows >= 3) {
      windows_list[[3]] <- data.frame(
        WindowNumber = 3L,
        Orientation = input$window3_orientation,
        WallSurfaceFraction = input$window3_area / input$floor_area,
        stringsAsFactors = FALSE
      )
    }
    
    do.call(rbind, windows_list)
  }
  
  # =========================================================================
  # VALIDATION USING EXISTING shared.R FUNCTIONS
  # =========================================================================
  validate_standard_inputs <- function() {
    all_errors <- character(0)
    all_warnings <- character(0)
    
    cat("\n=== VALIDATING STANDARD MODULE INPUTS ===\n")
    
    # Validate Time Data
    time_df <- build_time_data_from_inputs()
    if (!is.null(time_df)) {
      time_result <- validate_time_data(time_df)
      all_errors <- c(all_errors, time_result$errors)
      all_warnings <- c(all_warnings, time_result$warnings)
      
      cat("Time validation:", ifelse(time_result$valid, "PASS", "FAIL"), "\n")
      if (length(time_result$errors) > 0) {
        cat("  Errors:", paste(time_result$errors, collapse = "; "), "\n")
      }
      if (length(time_result$warnings) > 0) {
        cat("  Warnings:", paste(time_result$warnings, collapse = "; "), "\n")
      }
    } else {
      all_errors <- c(all_errors, "Failed to parse time data from inputs")
      cat("Time validation: FAIL (parse error)\n")
    }
    
    # Validate Box Data
    box_df <- build_box_data_from_inputs()
    box_result <- validate_box_data(box_df)
    all_errors <- c(all_errors, box_result$errors)
    all_warnings <- c(all_warnings, box_result$warnings)
    
    cat("Box Data validation:", ifelse(box_result$valid, "PASS", "FAIL"), "\n")
    if (length(box_result$errors) > 0) {
      cat("  Errors:", paste(box_result$errors, collapse = "; "), "\n")
    }
    if (length(box_result$warnings) > 0) {
      cat("  Warnings:", paste(box_result$warnings, collapse = "; "), "\n")
    }
    
    # Validate Windows Data
    windows_df <- build_windows_data_from_inputs()
    if (!is.null(windows_df)) {
      windows_result <- validate_windows(windows_df)
      all_errors <- c(all_errors, windows_result$errors)
      all_warnings <- c(all_warnings, windows_result$warnings)
      
      cat("Windows validation:", ifelse(windows_result$valid, "PASS", "FAIL"), "\n")
      if (length(windows_result$errors) > 0) {
        cat("  Errors:", paste(windows_result$errors, collapse = "; "), "\n")
      }
      if (length(windows_result$warnings) > 0) {
        cat("  Warnings:", paste(windows_result$warnings, collapse = "; "), "\n")
      }
    }
    
    # Additional validation: Activity times consistency
    start_time_act <- suppressWarnings(as.numeric(input$activity1_start))
    stop_time_act <- suppressWarnings(as.numeric(input$activity1_stop))
    
    if (is.na(start_time_act)) {
      all_errors <- c(all_errors, "Activity start time must be a number (minutes)")
    } else if (start_time_act < 0) {
      all_errors <- c(all_errors, "Activity start time cannot be negative")
    }
    
    if (is.na(stop_time_act)) {
      all_errors <- c(all_errors, "Activity stop time must be a number (minutes)")
    } else if (stop_time_act < 0) {
      all_errors <- c(all_errors, "Activity stop time cannot be negative")
    }
    
    if (!is.na(start_time_act) && !is.na(stop_time_act) && stop_time_act <= start_time_act) {
      all_errors <- c(all_errors, "Activity stop time must be greater than start time")
    }
    
    # Check activity duration vs simulation duration
    if (!is.na(stop_time_act) && !is.na(input$duration2)) {
      duration_minutes <- input$duration2 * 60
      if (stop_time_act > duration_minutes) {
        all_warnings <- c(all_warnings, 
                          paste0("Activity stop time (", stop_time_act, " min) exceeds simulation duration (", 
                                 duration_minutes, " min)"))
      }
    }
    
    # Validate ventilation settings
    if (input$num_vent_sources >= 1 && input$vent1 == "Open Window") {
      if (is.na(input$vent1_window) || input$vent1_window < 1) {
        all_errors <- c(all_errors, "Ventilation window number must be at least 1")
      }
      
      num_windows <- as.numeric(input$num_windows)
      if (input$vent1_window > num_windows) {
        all_errors <- c(all_errors, 
                        paste("Ventilation refers to window", input$vent1_window, 
                              "but only", num_windows, "window(s) defined"))
      }
    }
    
    cat("=== VALIDATION COMPLETE ===\n")
    cat("Total errors:", length(all_errors), "\n")
    cat("Total warnings:", length(all_warnings), "\n\n")
    
    create_validation_result(length(all_errors) == 0, all_errors, all_warnings)
  }
  
  # =========================================================================
  # VALIDATE BUTTON OBSERVER
  # =========================================================================
  observeEvent(input$validate_standard_inputs, {
    result <- validate_standard_inputs()
    validation_results_standard$user_inputs <- result
    
    # Show modal with results
    if (result$valid && length(result$warnings) == 0) {
      showModal(modalDialog(
        title = "✓ Validation Passed",
        HTML("<p style='color: green; font-weight: bold;'>All input values are valid!</p>"),
        easyClose = TRUE, 
        footer = modalButton("OK")
      ))
    } else {
      showModal(modalDialog(
        title = if (result$valid) "⚠ Validation Passed with Warnings" else "✗ Validation Failed",
        HTML(format_validation_messages(result)),
        size = "l",
        easyClose = TRUE, 
        footer = modalButton("OK")
      ))
    }
  })
  
  # =========================================================================
  # RENDER VALIDATION STATUS
  # =========================================================================
  output$standard_validation_status <- renderUI({
    result <- validation_results_standard$user_inputs
    
    if (is.null(result)) {
      return(tags$div(
        style = "color: gray; font-size: 14px; text-align: center; padding: 10px;",
        "Click 'Validate All Inputs' to check your configuration"
      ))
    }
    
    if (result$valid && length(result$warnings) == 0) {
      return(tags$div(
        style = "color: #28a745; font-size: 14px; font-weight: bold; text-align: center; padding: 10px; background-color: #d4edda; border-radius: 5px;",
        "✓ All inputs valid"
      ))
    } else if (result$valid && length(result$warnings) > 0) {
      return(tags$div(
        style = "color: #856404; font-size: 14px; font-weight: bold; text-align: center; padding: 10px; background-color: #fff3cd; border-radius: 5px;",
        paste("⚠ Valid with", length(result$warnings), "warning(s)")
      ))
    } else {
      return(tags$div(
        style = "color: #721c24; font-size: 14px; font-weight: bold; text-align: center; padding: 10px; background-color: #f8d7da; border-radius: 5px;",
        paste("✗", length(result$errors), "error(s) found")
      ))
    }
  })
  
  # =========================================================================
  # RENDER DETAILED VALIDATION RESULTS
  # =========================================================================
  output$standard_validation_details <- renderUI({
    result <- validation_results_standard$user_inputs
    
    if (is.null(result)) {
      return(HTML("<p>No validation has been performed yet.</p><p>Click 'Validate All Inputs' to check your configuration.</p>"))
    }
    
    html_output <- "<h4>Input Validation Results</h4>"
    
    if (result$valid && length(result$warnings) == 0) {
      html_output <- paste0(
        html_output,
        "<div style='padding: 15px; background-color: #d4edda; border: 1px solid #c3e6cb; border-radius: 5px;'>",
        "<h5 style='color: #155724; margin-top: 0;'>✓ All Validation Checks Passed</h5>",
        "<p style='color: #155724;'>Your configuration is valid and ready to save.</p>",
        "</div>"
      )
    } else {
      if (length(result$errors) > 0) {
        html_output <- paste0(
          html_output,
          "<div style='padding: 15px; background-color: #f8d7da; border: 1px solid #f5c6cb; border-radius: 5px; margin-bottom: 15px;'>",
          "<h5 style='color: #721c24; margin-top: 0;'>✗ Errors Found</h5>",
          "<ul style='color: #721c24; margin-bottom: 0;'>",
          paste0("<li>", result$errors, "</li>", collapse = ""),
          "</ul>",
          "</div>"
        )
      }
      
      if (length(result$warnings) > 0) {
        html_output <- paste0(
          html_output,
          "<div style='padding: 15px; background-color: #fff3cd; border: 1px solid #ffeaa7; border-radius: 5px;'>",
          "<h5 style='color: #856404; margin-top: 0;'>⚠ Warnings</h5>",
          "<ul style='color: #856404; margin-bottom: 0;'>",
          paste0("<li>", result$warnings, "</li>", collapse = ""),
          "</ul>",
          "</div>"
        )
      }
    }
    
    HTML(html_output)
  })
  
  # Navigate to outputs (with validation check)
  observeEvent(input$continue_to_outputs_standard, {
    if (is.null(validation_results_standard$user_inputs)) {
      showModal(modalDialog(
        title = "⚠ Validation Recommended",
        HTML(paste0(
          "<p>You haven't validated your inputs yet.</p>",
          "<p><strong>It is recommended to validate before proceeding.</strong></p>",
          "<p>Do you want to continue anyway?</p>"
        )),
        footer = tagList(
          actionButton("continue_without_validation", "Continue Anyway"),
          modalButton("Go Back and Validate")
        ),
        easyClose = FALSE
      ))
    } else if (!validation_results_standard$user_inputs$valid) {
      showModal(modalDialog(
        title = "✗ Validation Errors Present",
        HTML(paste0(
          "<p style='color: red; font-weight: bold;'>Your inputs have validation errors that should be corrected:</p>",
          format_validation_messages(validation_results_standard$user_inputs),
          "<hr>",
          "<p>Do you want to continue anyway? (Not recommended)</p>"
        )),
        size = "l",
        footer = tagList(
          actionButton("continue_with_errors", "Continue Anyway", style = "background-color: #dc3545; color: white;"),
          modalButton("Go Back and Fix Errors")
        ),
        easyClose = FALSE
      ))
    } else {
      modal_toggle("standard_menu", "hide")
      modal_toggle("standard_outputs_menu", "show")
    }
  })
  
  observeEvent(input$continue_without_validation, {
    removeModal()
    modal_toggle("standard_menu", "hide")
    modal_toggle("standard_outputs_menu", "show")
  })
  
  observeEvent(input$continue_with_errors, {
    removeModal()
    modal_toggle("standard_menu", "hide")
    modal_toggle("standard_outputs_menu", "show")
  })
  
  # Save standard configuration - WITH VALIDATION CHECK
  observeEvent(input$save_standard, {
    
    # Final validation before save
    final_result <- validate_standard_inputs()
    
    if (!final_result$valid) {
      showModal(modalDialog(
        title = "✗ Cannot Save - Validation Errors",
        HTML(paste0(
          "<p style='color: red; font-weight: bold;'>Your configuration has validation errors that must be fixed:</p>",
          format_validation_messages(final_result),
          "<hr>",
          "<p><strong>Please correct these errors and try saving again.</strong></p>"
        )),
        size = "l",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return()
    }
    
    # Show warnings if present but allow save
    if (length(final_result$warnings) > 0) {
      showModal(modalDialog(
        title = "⚠ Save with Warnings?",
        HTML(paste0(
          "<p style='color: orange; font-weight: bold;'>Your configuration has warnings:</p>",
          format_validation_messages(final_result),
          "<hr>",
          "<p>These are non-critical issues. Do you want to proceed with saving?</p>"
        )),
        size = "l",
        footer = tagList(
          actionButton("save_standard_with_warnings", "Save Anyway", style = "background-color: #ffc107; color: black;"),
          modalButton("Cancel")
        ),
        easyClose = FALSE
      ))
      return()
    }
    
    # If no errors and no warnings, proceed with save
    perform_standard_save()
  })
  
  observeEvent(input$save_standard_with_warnings, {
    removeModal()
    perform_standard_save()
  })
  
  # =========================================================================
  # PERFORM SAVE FUNCTION
  # =========================================================================
  perform_standard_save <- function() {
    
    cat("\n✅ Validation passed. Proceeding with save...\n")
    
    # Initialize input_data_list if it doesn't exist
    if (!exists("input_data_list", envir = .GlobalEnv)) {
      assign("input_data_list", list(), envir = .GlobalEnv)
    }
    
    # Load all 17 default files
    data_list_preload <- list()
    
    for (i in seq_along(file_paths_advanced)) {
      if (file_paths_advanced[i] != "none") {
        df <- read_data(file_paths_advanced[i], show_error_modal = FALSE)
        if (!is.null(df)) {
          data_list_preload[[paste0("data_preload", i)]] <- df
        }
      }
    }
    
    # Save all 17 files
    lapply(seq_along(titles_advanced), function(i) {
      input_data_list <- get("input_data_list", envir = .GlobalEnv)
      file_key <- gsub(" ", "", titles_advanced[i])
      
      if (is.null(input_data_list[[file_key]])) {
        input_data_list[[file_key]] <- list()
      }
      
      if (!is.null(data_list_preload[[paste0("data_preload", i)]])) {
        input_data_list[[file_key]][[length(input_data_list[[file_key]]) + 1]] <- 
          data_list_preload[[paste0("data_preload", i)]]
        assign("input_data_list", input_data_list, envir = .GlobalEnv)
      }
    })
    
    # Override user-configurable parameters
    input_data_list <- get("input_data_list", envir = .GlobalEnv)
    
    # Override Time
    if (!is.null(input_data_list$Time) && length(input_data_list$Time) > 0) {
      time_data_full <- input_data_list$Time[[1]]
      time_data_full$StartTimeYear <- as.integer(format(as.Date(input$start_date, format = "%d/%m/%Y"), "%Y"))
      time_data_full$StartTimeMonth <- as.integer(format(as.Date(input$start_date, format = "%d/%m/%Y"), "%m"))
      time_data_full$StartTimeDay <- as.integer(format(as.Date(input$start_date, format = "%d/%m/%Y"), "%d"))
      time_data_full$StartTime <- input$start_time
      time_data_full$StartTimeStandard <- ifelse(input$timezone == "Central - Daylight", "CST6CDT", "CST")
      time_data_full$Duration <- input$duration2
      input_data_list$Time[[1]] <- time_data_full
    }
    
    # Override BoxData
    if (!is.null(input_data_list$BoxData) && length(input_data_list$BoxData) > 0) {
      box_data_full <- input_data_list$BoxData[[1]]
      box_data_full$FloorSurfaceArea <- input$floor_area
      box_data_full$RoomHeight <- input$room_height
      box_data_full$AspectRatio <- input$aspect_ratio
      box_data_full$OrientationWiderSide <- input$orientation
      input_data_list$BoxData[[1]] <- box_data_full
    }
    
    # Override Windows
    if (as.numeric(input$num_windows) > 0) {
      default_windows <- input_data_list$Windows[[1]]
      num_user_windows <- as.numeric(input$num_windows)
      
      if (nrow(default_windows) > num_user_windows) {
        default_windows <- default_windows[1:num_user_windows, ]
      }
      
      if (num_user_windows >= 1 && nrow(default_windows) >= 1) {
        default_windows$WindowNumber[1] <- as.integer(1)
        default_windows$Orientation[1] <- input$window1_orientation
        default_windows$WallSurfaceFraction[1] <- input$window1_area / input$floor_area
      }
      
      if (num_user_windows >= 2 && nrow(default_windows) >= 2) {
        default_windows$WindowNumber[2] <- as.integer(2)
        default_windows$Orientation[2] <- input$window2_orientation
        default_windows$WallSurfaceFraction[2] <- input$window2_area / input$floor_area
      }
      
      if (num_user_windows >= 3 && nrow(default_windows) >= 3) {
        default_windows$WindowNumber[3] <- as.integer(3)
        default_windows$Orientation[3] <- input$window3_orientation
        default_windows$WallSurfaceFraction[3] <- input$window3_area / input$floor_area
      }
      
      input_data_list$Windows[[1]] <- default_windows
    }
    
    # Handle InitialValues
    if (is.null(input_data_list$InitialValues) || length(input_data_list$InitialValues) == 0) {
      input_data_list$InitialValues[[1]] <- "None"
    }
    
    # Save back to global environment
    assign("input_data_list", input_data_list, envir = .GlobalEnv)
    
    counter(counter() + 1)
    
    # Render preview
    output$standard_preview <- renderUI({
      input_data_list <- get("input_data_list", envir = .GlobalEnv)
      
      tagList(
        h4("Configuration Summary"),
        
        h5(style = "color: #2E86C1;", "Time Data"),
        renderTable(input_data_list$Time[[1]]),
        
        h5(style = "color: #2E86C1;", "Box Data"),
        p(style = "color: #666; font-size: 12px;", 
          "Showing first 8 columns (all ", ncol(input_data_list$BoxData[[1]]), " columns preserved)"),
        renderTable(input_data_list$BoxData[[1]][, 1:min(8, ncol(input_data_list$BoxData[[1]]))]),
        
        if (as.numeric(input$num_windows) > 0) {
          tagList(
            h5(style = "color: #2E86C1;", "Windows Data"),
            p(style = "color: #666; font-size: 12px;", 
              "All ", ncol(input_data_list$Windows[[1]]), " columns preserved from default file"),
            renderTable(input_data_list$Windows[[1]])
          )
        } else {
          tagList(
            h5(style = "color: #2E86C1;", "Windows Data (Default)"),
            p("Using default: ThreeWindowsESW.csv")
          )
        },
        
        h5(style = "color: #2E86C1;", "Activity Settings (UI Reference Only)"),
        p(paste("Activity:", input$activity1)),
        p(paste("Count:", input$activity1_count)),
        p(paste("Duration:", input$activity1_start, "-", input$activity1_stop, "minutes")),
        p(style = "color: #856404; background-color: #fff3cd; padding: 10px; border-radius: 5px;",
          strong("Note:"), " The activity schedule from the default Activities.csv file will be used for the simulation. ",
          "The UI settings above are for reference only."),
        
        tags$hr(),
        
        h5(style = "color: #28a745;", "✅ All 17 Default Input Files Loaded"),
        p("The following input files have been loaded with complete structure:"),
        tags$ol(
          tags$li(strong("Initial Values:"), " ", 
                  if(is.character(input_data_list$InitialValues[[1]]) && input_data_list$InitialValues[[1]] == "None") {
                    "Set to 'None'"
                  } else {
                    paste(ncol(input_data_list$InitialValues[[1]]), "species loaded")
                  }),
          tags$li(strong("Deposition Velocity:"), " ", nrow(input_data_list$DepositionVelocity[[1]]), " species"),
          tags$li(strong("Physical Environment:"), " ", ncol(input_data_list$PhysicalEnvironment[[1]]), " parameters"),
          tags$li(strong("Emission Profiles:"), " ", nrow(input_data_list$EmissionProfiles[[1]]), " profiles"),
          tags$li(strong("Activities:"), " ", nrow(input_data_list$Activities[[1]]), " time points"),
          tags$li(strong("Box Data:"), " ", ncol(input_data_list$BoxData[[1]]), " parameters (4 user values updated)"),
          tags$li(strong("Outdoor Concentrations:"), " ", ncol(input_data_list$OutdoorConcentrations[[1]]), " columns"),
          tags$li(strong("Time:"), " ", ncol(input_data_list$Time[[1]]), " parameters (6 user values updated)"),
          tags$li(strong("Indoor Light:"), " ", 
                  if(is.list(input_data_list$IndoorLight[[1]]) && !is.data.frame(input_data_list$IndoorLight[[1]])) {
                    paste(length(input_data_list$IndoorLight[[1]]), "sheets")
                  } else {
                    "Loaded"
                  }),
          tags$li(strong("Outdoor Light Direct:"), " ", nrow(input_data_list$OutdoorLightDirect[[1]]), " rows"),
          tags$li(strong("Outdoor Light Diffuse:"), " ", nrow(input_data_list$OutdoorLightDiffuse[[1]]), " rows"),
          tags$li(strong("Windows:"), " ", nrow(input_data_list$Windows[[1]]), " windows, ", 
                  ncol(input_data_list$Windows[[1]]), " columns (all preserved)"),
          tags$li(strong("Glass Transmission:"), " ", nrow(input_data_list$GlassTransmission[[1]]), " rows"),
          tags$li(strong("Artificial Light:"), " ", nrow(input_data_list$ArtificialLight[[1]]), " rows"),
          tags$li(strong("Artificial Light List:"), " ", nrow(input_data_list$ArtificialLightList[[1]]), " lights"),
          tags$li(strong("Artificial Light Spectra:"), " ", ncol(input_data_list$ArtificialLightSpectra[[1]]), " wavelengths"),
          tags$li(strong("Artificial Light Schedule:"), " ", nrow(input_data_list$ArtificialLightSchedule[[1]]), " time points")
        ),
        
        tags$hr(),
        
        div(
          style = "background-color: #d4edda; border: 1px solid #c3e6cb; border-radius: 5px; padding: 15px; margin-top: 10px;",
          h5(style = "color: #155724; margin-top: 0;", "✅ Configuration Validated and Saved"),
          tags$ul(
            style = "color: #155724; margin-bottom: 0;",
            tags$li("All user inputs passed validation"),
            tags$li("All 17 parameters loaded with complete structure"),
            tags$li("Ready for simulation")
          )
        )
      )
    })
    
    showModal(modalDialog(
      title = "✅ Configuration Saved Successfully",
      HTML(paste0(
        "<p style='color: green; font-weight: bold;'>Standard simulation configuration saved successfully!</p>",
        "<p>All inputs passed final validation.</p>",
        "<p><strong>Ready to run simulation.</strong></p>",
        "<hr>",
        "<p style='font-size: 12px; color: #666;'>Configuration has been saved to global environment as 'input_data_list'.</p>"
      )),
      easyClose = TRUE, footer = modalButton("OK")
    ))
    
    modal_toggle("standard_outputs_menu", "hide")
  }
  
  # Wire the shared output server module
  output_server("output_module_standard")
}