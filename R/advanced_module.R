# advanced_module.R
# =======================================================================
# Advanced simulation module - WITH MANUAL EDIT VALIDATION
# =======================================================================

# -------------------------
# Advanced UI
# -------------------------
advanced_module_ui <- function() {
  tagList(
    # Advanced Inputs Modal
    modal_ui(
      id    = "advanced_inputs_menu",
      title = "Advanced Simulation - Input Files",
      size  = "xl",
      fluidRow(
        column(
          4,
          div(
            style = "max-height: 90vh; overflow-y: auto;",
            wellPanel(
              actionButton(
                "preload_all_files", "Preload All Default Input Files",
                style = "background-color: #17a2b8; color: white; font-weight: bold; width: 100%; margin-bottom: 10px;"
              ),
              actionButton(
                "validate_all_files", "Validate All Input Files",
                style = "background-color: #28a745; color: white; font-weight: bold; width: 100%; margin-bottom: 10px;"
              ),
              actionButton(
                "show_file_structure", "Show File Structure (Diagnostic)",
                style = "background-color: #6c757d; color: white; font-weight: bold; width: 100%; margin-bottom: 10px;"
              ),
              helpText("Click above to automatically preload all default input files at once, or configure each file individually below."),
              div(
                id = "validation_summary_link",
                style = "margin-bottom: 10px; padding: 10px; background-color: #f8f9fa; border-radius: 5px; text-align: center;",
                HTML("<strong>View validation results in the 'Validation Summary' tab →</strong>")
              ),
              
              # Group 1: Environment Setup
              h4("Environment Setup"),
              lapply(c(1, 3, 6, 7), function(i) render_input_block(i)),
              
              # Group 2: Source & Activity Setup
              h4("Source & Activity Setup"),
              lapply(c(4, 5, 2), function(i) render_input_block(i)),
              
              # Group 3: Light & Exposure
              h4("Light & Exposure"),
              lapply(c(9, 10, 11, 12, 13, 14, 15, 16, 17), function(i) render_input_block(i)),
              
              # Group 4: Simulation Timing
              h4("Simulation Timing"),
              render_input_block(8),
              
              tags$hr(),
              actionButton("continue_to_outputs", "Continue to Output Settings")
            )
          )
        ),
        column(
          8,
          div(
            style = "max-height: 90vh; overflow-y: auto;",
            tabsetPanel(
              id = "preview_tabs",
              tabPanel("Data Preview", uiOutput("sheet_tabs"), value = "data_preview"),
              tabPanel("Validation Summary", div(style = "padding: 15px;", htmlOutput("validation_summary")), value = "validation_summary")
            )
          )
        )
      )
    ),
    
    # Advanced Outputs Modal
    modal_ui(
      id    = "advanced_outputs_menu",
      title = "Advanced Simulation - Output Settings",
      size  = "l",
      fluidRow(
        column(
          12,
          wellPanel(
            h4("Output Settings"),
            output_ui("output_module_advanced"),
            tags$hr(),
            div(style="padding:12px;background:#eaf4fb;border-radius:5px;border-left:4px solid #2E86C1;margin-bottom:12px;",
              radioButtons("adv_mechanism", "Chemical Mechanism",
                choiceNames  = c("SAPRC99 (default)", "SAPRC07T"),
                choiceValues = c("SAPRC99", "SAPRC07T"),
                selected = "SAPRC99", inline = TRUE)
            ),
            actionButton("save_advanced", "Save Advanced Simulation")
          )
        )
      )
    )
  )
}

# -------------------------
# Advanced Server
# -------------------------
advanced_module_server <- function(input, output, session, get_duration) {
  
  # ---- Reactive storage & helpers ----
  validation_results   <- reactiveValues()
  data_list_advanced   <- reactiveValues()
  counter_advanced     <- reactiveVal(1)
  current_file_index   <- reactiveVal(NULL)  # Track which file is being previewed
  
  # helper to read CSV/XLSX, first sheet if multiple
  read_data <- function(file_path, show_error_modal = TRUE) {
    tryCatch({
      if (grepl("\\.csv$", file_path, ignore.case = TRUE)) {
        df <- read.csv(file_path, comment.char = "#", na.strings = "None")
        if (nrow(df) == 0 || (nrow(df) == 1 && all(is.na(df[1, ])))) return(data.frame())
        return(df)
        
      } else if (grepl("\\.xlsx$", file_path, ignore.case = TRUE)) {
        sheets <- excel_sheets(file_path)
        if (length(sheets) == 1) {
          df <- tryCatch(read_excel(file_path), error = function(e) read_excel(file_path, comment = "#"))
          if (nrow(df) == 0 || (nrow(df) == 1 && all(is.na(df[1, ])))) return(data.frame())
          return(list(df))
        } else {
          df_list <- lapply(sheets, function(sheet) read_excel(file_path, sheet = sheet))
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
  
  # Render tabs for preview (single or multi-sheet) - WITH EDIT CAPTURE
  render_sheet_tabs <- function(df_list, i, read_only = FALSE) {
    current_file_index(i)  # Track which file is being edited
    
    output$sheet_tabs <- renderUI({
      if (is.list(df_list) && !is.data.frame(df_list)) {
        do.call(tabsetPanel, c(
          id = paste0("tabs_", i),
          lapply(names(df_list), function(sheet) {
            tabPanel(title = sheet, rHandsontableOutput(outputId = paste0("table_", i, "_", sheet)))
          })
        ))
      } else {
        tagList(rHandsontableOutput("table_advanced"))
      }
    })
    
    if (is.list(df_list) && !is.data.frame(df_list)) {
      lapply(names(df_list), function(sheet) {
        local({
          sheet_name <- sheet
          output[[paste0("table_", i, "_", sheet_name)]] <- renderRHandsontable({
            rhandsontable(df_list[[sheet_name]], readOnly = read_only)
          })
        })
      })
    } else {
      output$table_advanced <- renderRHandsontable({
        rhandsontable(df_list, readOnly = read_only)
      })
      
      # CAPTURE MANUAL EDITS - Update the data when user changes values
      if (!read_only) {
        observeEvent(input$table_advanced, {
          req(input$table_advanced)
          edited_df <- hot_to_r(input$table_advanced)
          
          # Determine which data source to update based on what was loaded
          if (!is.null(data_list_advanced[[paste0("data", i)]])) {
            data_list_advanced[[paste0("data", i)]] <- edited_df
          } else if (!is.null(data_list_advanced[[paste0("data_online", i)]])) {
            data_list_advanced[[paste0("data_online", i)]] <- edited_df
          } else if (!is.null(data_list_advanced[[paste0("data_preload", i)]])) {
            data_list_advanced[[paste0("data_preload", i)]] <- edited_df
          } else if (!is.null(data_list_advanced[[paste0("data_create", i)]])) {
            data_list_advanced[[paste0("data_create", i)]] <- edited_df
          }
          
          # Clear validation result since data changed
          validation_results[[paste0("result_", i)]] <- NULL
        }, ignoreInit = TRUE)
      }
    }
  }
  
  # ---- Per-file observers: uploads/imports/preloads/validation/preview ----
  lapply(seq_along(titles_advanced), function(i) {
    
    # Upload
    observeEvent(input[[paste0("file", i)]], {
      req(input[[paste0("file", i)]])
      df <- read_data(input[[paste0("file", i)]]$datapath)
      if (!is.null(df)) {
        data_list_advanced[[paste0("data", i)]] <- df
        # Auto-validate for types with specific validators
        if (i %in% c(1:8, 12)) {
          df_to_validate <- if (is.list(df) && !is.data.frame(df)) df[[1]] else df
          result <- validate_data_file(df_to_validate, file_type_map[i], get_duration())
          validation_results[[paste0("result_", i)]] <- result
        }
      }
    })
    
    # Import (URL or local path)
    observeEvent(input[[paste0("import_table", i)]], {
      req(input[[paste0("url", i)]])
      df <- read_data(input[[paste0("url", i)]])
      if (!is.null(df)) {
        data_list_advanced[[paste0("data_online", i)]] <- df
      }
    })
    
    # Preload from defaults
    observeEvent(input[[paste0("preload_table", i)]], {
      if (file_paths_advanced[i] != "none") {
        df <- read_data(file_paths_advanced[i])
        if (!is.null(df)) {
          data_list_advanced[[paste0("data_preload", i)]] <- df
        }
      } else {
        data_list_advanced[[paste0("data_preload", i)]] <- data.frame(Column1 = numeric(0), Column2 = numeric(0))
      }
    })
    
    # Validate (uploaded)
    observeEvent(input[[paste0("validate", i)]], {
      df <- data_list_advanced[[paste0("data", i)]]
      if (is.null(df)) {
        showModal(modalDialog(
          title = "Validation Error",
          "No data loaded for validation. Please upload or preload data first.",
          easyClose = TRUE, footer = modalButton("OK")
        ))
        return()
      }
      df_to_validate <- if (is.list(df) && !is.data.frame(df)) df[[1]] else df
      result <- validate_data_file(df_to_validate, file_type_map[i], get_duration())
      validation_results[[paste0("result_", i)]] <- result
      
      if (result$valid && length(result$warnings) == 0) {
        showModal(modalDialog(
          title = "Validation Passed",
          HTML(paste0("<p style='color: green; font-weight: bold;'>", titles_advanced[i], 
                      " passed all validation checks.</p>")),
          easyClose = TRUE, footer = modalButton("OK")
        ))
      } else {
        showModal(modalDialog(
          title = paste("Validation Results:", titles_advanced[i]),
          HTML(format_validation_messages(result)),
          easyClose = TRUE, footer = modalButton("OK")
        ))
      }
    })
    
    # Validate (imported)
    observeEvent(input[[paste0("validate_import", i)]], {
      df <- data_list_advanced[[paste0("data_online", i)]]
      if (is.null(df)) {
        showModal(modalDialog(
          title = "Validation Error", 
          "No data loaded for validation.", 
          easyClose = TRUE, footer = modalButton("OK")
        ))
        return()
      }
      df_to_validate <- if (is.list(df) && !is.data.frame(df)) df[[1]] else df
      result <- validate_data_file(df_to_validate, file_type_map[i], get_duration())
      validation_results[[paste0("result_", i)]] <- result
      
      if (result$valid && length(result$warnings) == 0) {
        showModal(modalDialog(
          title = "Validation Passed",
          HTML(paste0("<p style='color: green; font-weight: bold;'>", titles_advanced[i], 
                      " passed all validation checks.</p>")),
          easyClose = TRUE, footer = modalButton("OK")
        ))
      } else {
        showModal(modalDialog(
          title = paste("Validation Results:", titles_advanced[i]),
          HTML(format_validation_messages(result)),
          easyClose = TRUE, footer = modalButton("OK")
        ))
      }
    })
    
    # --- Preview buttons ---
    observeEvent(input[[paste0("show_table", i)]], {
      df <- data_list_advanced[[paste0("data", i)]]
      if (!is.null(df)) render_sheet_tabs(df, i, read_only = FALSE)
    })
    observeEvent(input[[paste0("import_table", i)]], {
      df <- data_list_advanced[[paste0("data_online", i)]]
      if (!is.null(df)) render_sheet_tabs(df, i, read_only = FALSE)
    })
    observeEvent(input[[paste0("preload_table", i)]], {
      df <- data_list_advanced[[paste0("data_preload", i)]]
      if (!is.null(df)) render_sheet_tabs(df, i, read_only = FALSE)
    })
    observeEvent(input[[paste0("create_table", i)]], {
      df <- data_list_advanced[[paste0("data_create", i)]]
      if (!is.null(df)) render_sheet_tabs(df, i, read_only = FALSE)
    })
    observeEvent(input[[paste0("preview_preload", i)]], {
      df <- data_list_advanced[[paste0("data_preload", i)]]
      if (!is.null(df)) render_sheet_tabs(df, i, read_only = TRUE)
    })
    observeEvent(input[[paste0("preview_import", i)]], {
      df <- data_list_advanced[[paste0("data_online", i)]]
      if (!is.null(df)) render_sheet_tabs(df, i, read_only = TRUE)
    })
    observeEvent(input[[paste0("preview_create", i)]], {
      df <- data_list_advanced[[paste0("data_create", i)]]
      if (!is.null(df)) render_sheet_tabs(df, i, read_only = TRUE)
    })
    
    # ---- Validation status indicator (used by render_input_block) ----
    output[[paste0("validation_status_", i)]] <- renderUI({
      result <- validation_results[[paste0("result_", i)]]
      if (is.null(result)) {
        return(tags$div(style = "color: gray; font-size: 12px;", "Not validated"))
      }
      if (result$valid && length(result$warnings) == 0) {
        return(tags$div(style = "color: green; font-size: 12px; font-weight: bold;", "✓ Valid"))
      } else if (result$valid && length(result$warnings) > 0) {
        return(tags$div(style = "color: orange; font-size: 12px; font-weight: bold;", "⚠ Valid with warnings"))
      } else {
        return(tags$div(style = "color: red; font-size: 12px; font-weight: bold;", "✗ Errors found"))
      }
    })
  })
  
  # ---- Batch validate ----
  observeEvent(input$validate_all_files, {
    validated_count <- 0
    error_count     <- 0
    warning_count   <- 0
    
    for (i in seq_along(titles_advanced)) {
      # Check all possible data sources in priority order
      df <- data_list_advanced[[paste0("data", i)]]
      if (is.null(df)) df <- data_list_advanced[[paste0("data_online", i)]]
      if (is.null(df)) df <- data_list_advanced[[paste0("data_preload", i)]]
      if (is.null(df)) df <- data_list_advanced[[paste0("data_create", i)]]
      
      if (!is.null(df)) {
        df_to_validate <- if (is.list(df) && !is.data.frame(df)) df[[1]] else df
        result <- validate_data_file(df_to_validate, file_type_map[i], get_duration())
        validation_results[[paste0("result_", i)]] <- result
        validated_count <- validated_count + 1
        if (!result$valid) error_count   <- error_count + 1
        if (length(result$warnings) > 0) warning_count <- warning_count + 1
      }
    }
    
    updateTabsetPanel(session, "preview_tabs", selected = "validation_summary")
    showModal(modalDialog(
      title = "Batch Validation Complete",
      HTML(paste0(
        "<p>Validated ", validated_count, " files.</p>",
        "<p style='color: red;'>Files with errors: ", error_count, "</p>",
        "<p style='color: orange;'>Files with warnings: ", warning_count, "</p>",
        "<p><strong>The Validation Summary tab (on the right) now shows detailed results.</strong></p>",
        "<p style='font-size: 12px; color: #666;'>Note: If you manually edited values in the preview tables, ",
        "those changes have been captured and validated.</p>"
      )),
      easyClose = TRUE, footer = modalButton("OK")
    ))
  })
  
  # ---- File structure diagnostic ----
  observeEvent(input$show_file_structure, {
    file_structure <- ""
    for (i in seq_along(titles_advanced)) {
      # Check all possible data sources
      df <- data_list_advanced[[paste0("data", i)]]
      if (is.null(df)) df <- data_list_advanced[[paste0("data_online", i)]]
      if (is.null(df)) df <- data_list_advanced[[paste0("data_preload", i)]]
      if (is.null(df)) df <- data_list_advanced[[paste0("data_create", i)]]
      
      if (!is.null(df)) {
        df_to_check <- if (is.list(df) && !is.data.frame(df)) df[[1]] else df
        file_structure <- paste0(
          file_structure,
          "<div style='margin-bottom: 15px; padding: 10px; border: 1px solid #ddd; border-radius: 5px;'>",
          "<strong>", titles_advanced[i], "</strong><br>",
          "<em>Columns (", ncol(df_to_check), "):</em> ",
          paste(names(df_to_check), collapse = ", "),
          "<br><em>Rows:</em> ", nrow(df_to_check),
          "</div>"
        )
      }
    }
    showModal(modalDialog(
      title = "File Structure Diagnostic",
      HTML(paste0("<div style='max-height: 60vh; overflow-y: auto;'>", file_structure, "</div>")),
      size = "l",
      easyClose = TRUE, footer = modalButton("Close")
    ))
  })
  
  # ---- Validation summary panel ----
  output$validation_summary <- renderUI({
    results_list <- reactiveValuesToList(validation_results)
    if (length(results_list) == 0) {
      return(HTML("<p>No files have been validated yet.</p><p>Click 'Validate All Input Files' or validate individual files.</p>"))
    }
    
    html_output <- "<h4>Validation Summary</h4>"
    for (i in seq_along(titles_advanced)) {
      result <- validation_results[[paste0("result_", i)]]
      if (!is.null(result)) {
        status_class <- if (result$valid && length(result$warnings) == 0) {
          "validation-pass"
        } else if (result$valid && length(result$warnings) > 0) {
          "validation-warning"
        } else { 
          "validation-error" 
        }
        
        status_text <- if (result$valid && length(result$warnings) == 0) {
          "VALID"
        } else if (result$valid && length(result$warnings) > 0) {
          "VALID (with warnings)"
        } else { 
          "ERRORS DETECTED" 
        }
        
        html_output <- paste0(
          html_output, 
          "<div style='margin-bottom: 15px; padding: 10px; border: 1px solid #ddd; border-radius: 5px;'>",
          "<strong><span class='", status_class, "'>[", status_text, "]</span> ", 
          titles_advanced[i], "</strong><br>"
        )
        
        if (length(result$errors) > 0) {
          html_output <- paste0(
            html_output, 
            "<div style='color: #dc3545; margin-top: 5px;'>",
            "<strong>Errors:</strong><ul style='margin: 5px 0;'>",
            paste0("<li>", result$errors, "</li>", collapse = ""),
            "</ul></div>"
          )
        }
        
        if (length(result$warnings) > 0) {
          html_output <- paste0(
            html_output, 
            "<div style='color: #ffc107; margin-top: 5px;'>",
            "<strong>Warnings:</strong><ul style='margin: 5px 0;'>",
            paste0("<li>", result$warnings, "</li>", collapse = ""),
            "</ul></div>"
          )
        }
        
        if (result$valid && length(result$warnings) == 0) {
          html_output <- paste0(
            html_output, 
            "<div style='color: #28a745; margin-top: 5px;'>All validation checks passed.</div>"
          )
        }
        
        html_output <- paste0(html_output, "</div>")
      }
    }
    HTML(html_output)
  })
  
  # ---- Navigate to outputs (with validation check) ----
  observeEvent(input$continue_to_outputs, {
    critical_files <- c(8, 6, 3, 7)  # Time, BoxData, PhysicalEnvironment, OutdoorConcentrations
    missing_validations <- c()
    for (i in critical_files) {
      if (is.null(validation_results[[paste0("result_", i)]])) {
        missing_validations <- c(missing_validations, titles_advanced[i])
      }
    }
    if (length(missing_validations) > 0) {
      showModal(modalDialog(
        title = "Validation Warning",
        HTML(paste0(
          "<p>The following critical input files have not been validated:</p><ul>",
          paste0("<li>", missing_validations, "</li>", collapse = ""),
          "</ul><p>It is recommended to validate all inputs before proceeding.</p>",
          "<p><strong>Note:</strong> If you manually edited values in preview tables, ",
          "click 'Validate All Input Files' to validate your changes.</p>",
          "<p>Do you want to continue anyway?</p>"
        )),
        footer = tagList(
          actionButton("continue_anyway", "Continue Anyway"),
          modalButton("Go Back")
        ),
        easyClose = FALSE
      ))
    } else {
      modal_toggle("advanced_inputs_menu", "hide")
      modal_toggle("advanced_outputs_menu", "show")
    }
  })
  
  observeEvent(input$continue_anyway, {
    removeModal()
    modal_toggle("advanced_inputs_menu", "hide")
    modal_toggle("advanced_outputs_menu", "show")
  })
  
  # ---- Save Advanced configuration - WITH COMPREHENSIVE FINAL VALIDATION ----
  observeEvent(input$save_advanced, {
    
    # =========================================================================
    # STEP 1: COLLECT ALL DATA THAT WILL BE SAVED (including manual edits)
    # =========================================================================
    data_to_save <- list()
    
    for (i in seq_along(titles_advanced)) {
      # Check ALL data sources in priority order:
      # Priority reflects what user intends: uploaded > online > preloaded > created
      if (!is.null(data_list_advanced[[paste0("data", i)]])) {
        data_to_save[[i]] <- data_list_advanced[[paste0("data", i)]]
      } else if (!is.null(data_list_advanced[[paste0("data_online", i)]])) {
        data_to_save[[i]] <- data_list_advanced[[paste0("data_online", i)]]
      } else if (!is.null(data_list_advanced[[paste0("data_preload", i)]])) {
        data_to_save[[i]] <- data_list_advanced[[paste0("data_preload", i)]]
      } else if (!is.null(data_list_advanced[[paste0("data_create", i)]])) {
        data_to_save[[i]] <- data_list_advanced[[paste0("data_create", i)]]
      } else {
        data_to_save[[i]] <- NULL
      }
    }
    
    # =========================================================================
    # STEP 2: VALIDATE THE FINAL DATA BEFORE SAVING (including manual edits)
    # =========================================================================
    cat("\n=== VALIDATING FINAL DATA BEFORE SAVE (including manual edits) ===\n")
    
    final_validation_results <- list()
    critical_files <- c(8, 6, 3, 7, 1, 2, 4, 5, 12)  # All files that have validators
    files_with_errors <- c()
    files_with_warnings <- c()
    
    for (i in critical_files) {
      if (!is.null(data_to_save[[i]])) {
        # Extract dataframe for validation
        df_to_validate <- if (is.list(data_to_save[[i]]) && !is.data.frame(data_to_save[[i]])) {
          data_to_save[[i]][[1]]
        } else {
          data_to_save[[i]]
        }
        
        # Run validation
        result <- validate_data_file(df_to_validate, file_type_map[i], get_duration())
        final_validation_results[[i]] <- result
        
        # Update the validation_results reactive for display
        validation_results[[paste0("result_", i)]] <- result
        
        # Track results
        if (!result$valid) {
          files_with_errors <- c(files_with_errors, titles_advanced[i])
          cat(sprintf("❌ %s: FAILED validation\n", titles_advanced[i]))
          cat("Errors:\n")
          for (err in result$errors) {
            cat(sprintf("  - %s\n", err))
          }
        } else if (length(result$warnings) > 0) {
          files_with_warnings <- c(files_with_warnings, titles_advanced[i])
          cat(sprintf("⚠️  %s: Valid with warnings\n", titles_advanced[i]))
          cat("Warnings:\n")
          for (warn in result$warnings) {
            cat(sprintf("  - %s\n", warn))
          }
        } else {
          cat(sprintf("✅ %s: Valid\n", titles_advanced[i]))
        }
      } else {
        cat(sprintf("⚪ %s: No data provided\n", titles_advanced[i]))
      }
    }
    
    # =========================================================================
    # STEP 3: BLOCK SAVE IF CRITICAL ERRORS FOUND
    # =========================================================================
    if (length(files_with_errors) > 0) {
      # Switch to validation summary tab
      updateTabsetPanel(session, "preview_tabs", selected = "validation_summary")
      
      # Build detailed error message
      error_details <- ""
      for (file_name in files_with_errors) {
        idx <- which(titles_advanced == file_name)
        if (length(idx) > 0) {
          result <- final_validation_results[[idx]]
          if (!is.null(result) && length(result$errors) > 0) {
            error_details <- paste0(
              error_details,
              "<div style='margin-top: 10px; padding: 10px; background-color: #f8d7da; border-radius: 5px;'>",
              "<strong>", file_name, ":</strong><ul style='margin: 5px 0;'>",
              paste0("<li>", result$errors, "</li>", collapse = ""),
              "</ul></div>"
            )
          }
        }
      }
      
      showModal(modalDialog(
        title = "❌ Validation Errors Detected - Save Blocked",
        HTML(paste0(
          "<p style='color: red; font-weight: bold;'>The following files have validation errors and must be corrected:</p>",
          error_details,
          "<hr>",
          "<p><strong>Actions required:</strong></p>",
          "<ol>",
          "<li>Review the errors in the 'Validation Summary' tab (now open)</li>",
          "<li>If you manually entered wrong values, click 'Preview' to edit the table again</li>",
          "<li>Correct the errors in your data</li>",
          "<li>Click 'Validate All Input Files' to verify your corrections</li>",
          "<li>Try saving again</li>",
          "</ol>",
          if (length(files_with_warnings) > 0) {
            paste0(
              "<hr>",
              "<p style='color: orange;'><strong>Note:</strong> The following files also have warnings (non-blocking):</p>",
              "<ul>", paste0("<li>", files_with_warnings, "</li>", collapse = ""), "</ul>"
            )
          } else {
            ""
          }
        )),
        size = "l",
        easyClose = TRUE, 
        footer = modalButton("OK")
      ))
      return()
    }
    
    # =========================================================================
    # STEP 4: SHOW WARNINGS IF PRESENT (but allow save)
    # =========================================================================
    if (length(files_with_warnings) > 0) {
      # Build warning details
      warning_details <- ""
      for (file_name in files_with_warnings) {
        idx <- which(titles_advanced == file_name)
        if (length(idx) > 0) {
          result <- final_validation_results[[idx]]
          if (!is.null(result) && length(result$warnings) > 0) {
            warning_details <- paste0(
              warning_details,
              "<div style='margin-top: 10px; padding: 10px; background-color: #fff3cd; border-radius: 5px;'>",
              "<strong>", file_name, ":</strong><ul style='margin: 5px 0;'>",
              paste0("<li>", result$warnings, "</li>", collapse = ""),
              "</ul></div>"
            )
          }
        }
      }
      
      showModal(modalDialog(
        title = "⚠️ Validation Warnings - Review Before Saving",
        HTML(paste0(
          "<p style='color: orange; font-weight: bold;'>The following files have warnings:</p>",
          warning_details,
          "<hr>",
          "<p>Warnings do not block saving, but you may want to review them.</p>",
          "<p><strong>Do you want to proceed with saving?</strong></p>"
        )),
        size = "l",
        footer = tagList(
          actionButton("save_with_warnings", "Save Anyway", 
                       style = "background-color: #ffc107; color: black;"),
          modalButton("Cancel")
        ),
        easyClose = FALSE
      ))
      
      # Wait for user decision
      return()
    }
    
    # If no errors and no warnings, proceed directly to save
    perform_save(data_to_save)
  })
  
  # Handle save with warnings
  observeEvent(input$save_with_warnings, {
    removeModal()
    
    # Recollect data to save
    data_to_save <- list()
    for (i in seq_along(titles_advanced)) {
      if (!is.null(data_list_advanced[[paste0("data", i)]])) {
        data_to_save[[i]] <- data_list_advanced[[paste0("data", i)]]
      } else if (!is.null(data_list_advanced[[paste0("data_online", i)]])) {
        data_to_save[[i]] <- data_list_advanced[[paste0("data_online", i)]]
      } else if (!is.null(data_list_advanced[[paste0("data_preload", i)]])) {
        data_to_save[[i]] <- data_list_advanced[[paste0("data_preload", i)]]
      } else if (!is.null(data_list_advanced[[paste0("data_create", i)]])) {
        data_to_save[[i]] <- data_list_advanced[[paste0("data_create", i)]]
      } else {
        data_to_save[[i]] <- NULL
      }
    }
    
    perform_save(data_to_save)
  })
  
  # =========================================================================
  # PERFORM_SAVE FUNCTION - Extracted for reuse (MATCHES ORIGINAL LOGIC)
  # =========================================================================
  perform_save <- function(data_to_save) {
    cat("\n✅ All validation passed or warnings accepted. Proceeding with save...\n")
    
    # Save each file to global environment - EXACTLY as original
    lapply(seq_along(titles_advanced), function(i) {
      
      # Check uploaded data first
      if (!is.null(data_list_advanced[[paste0("data", i)]])) {
        base_name <- paste0("input_", gsub(" ", "", titles_advanced[i]))
        index <- counter_advanced()
        
        # Create unique name if needed (though typically index = 1)
        while (exists(paste0(base_name, "_", index), envir = .GlobalEnv)) {
          index <- index + 1
        }
        
        # Initialize input_data_list if it doesn't exist
        if (!exists("input_data_list", envir = .GlobalEnv)) {
          assign("input_data_list", list(), envir = .GlobalEnv)
        }
        
        # Get current list
        input_data_list <- get("input_data_list", envir = .GlobalEnv)
        
        # Initialize the specific file type list if it doesn't exist
        file_key <- gsub(" ", "", titles_advanced[i])
        if (is.null(input_data_list[[file_key]])) {
          input_data_list[[file_key]] <- list()
        }
        
        # Append data to the list
        input_data_list[[file_key]][[length(input_data_list[[file_key]]) + 1]] <- 
          data_list_advanced[[paste0("data", i)]]
        
        # Save back to global environment
        assign("input_data_list", input_data_list, envir = .GlobalEnv)
      }
      
      # Check online imported data
      if (!is.null(data_list_advanced[[paste0("data_online", i)]])) {
        base_name <- paste0("input_", gsub(" ", "", titles_advanced[i]))
        index <- counter_advanced()
        
        while (exists(paste0(base_name, "_", index), envir = .GlobalEnv)) {
          index <- index + 1
        }
        
        if (!exists("input_data_list", envir = .GlobalEnv)) {
          assign("input_data_list", list(), envir = .GlobalEnv)
        }
        
        input_data_list <- get("input_data_list", envir = .GlobalEnv)
        file_key <- gsub(" ", "", titles_advanced[i])
        
        if (is.null(input_data_list[[file_key]])) {
          input_data_list[[file_key]] <- list()
        }
        
        input_data_list[[file_key]][[length(input_data_list[[file_key]]) + 1]] <- 
          data_list_advanced[[paste0("data_online", i)]]
        
        assign("input_data_list", input_data_list, envir = .GlobalEnv)
      }
      
      # Check preloaded data
      if (!is.null(data_list_advanced[[paste0("data_preload", i)]])) {
        base_name <- paste0("input_", gsub(" ", "", titles_advanced[i]))
        index <- counter_advanced()
        
        while (exists(paste0(base_name, "_", index), envir = .GlobalEnv)) {
          index <- index + 1
        }
        
        if (!exists("input_data_list", envir = .GlobalEnv)) {
          assign("input_data_list", list(), envir = .GlobalEnv)
        }
        
        input_data_list <- get("input_data_list", envir = .GlobalEnv)
        file_key <- gsub(" ", "", titles_advanced[i])
        
        if (is.null(input_data_list[[file_key]])) {
          input_data_list[[file_key]] <- list()
        }
        
        input_data_list[[file_key]][[length(input_data_list[[file_key]]) + 1]] <- 
          data_list_advanced[[paste0("data_preload", i)]]
        
        assign("input_data_list", input_data_list, envir = .GlobalEnv)
      }
      
      # Check created data
      if (!is.null(data_list_advanced[[paste0("data_create", i)]])) {
        base_name <- paste0("input_", gsub(" ", "", titles_advanced[i]))
        index <- counter_advanced()
        
        while (exists(paste0(base_name, "_", index), envir = .GlobalEnv)) {
          index <- index + 1
        }
        
        if (!exists("input_data_list", envir = .GlobalEnv)) {
          assign("input_data_list", list(), envir = .GlobalEnv)
        }
        
        input_data_list <- get("input_data_list", envir = .GlobalEnv)
        file_key <- gsub(" ", "", titles_advanced[i])
        
        if (is.null(input_data_list[[file_key]])) {
          input_data_list[[file_key]] <- list()
        }
        
        input_data_list[[file_key]][[length(input_data_list[[file_key]]) + 1]] <- 
          data_list_advanced[[paste0("data_create", i)]]
        
        assign("input_data_list", input_data_list, envir = .GlobalEnv)
      }
    })
    
    # Ensure InitialValues exists - EXACTLY as original
    input_data_list <- get("input_data_list", envir = .GlobalEnv)
    if (is.null(input_data_list$InitialValues) || length(input_data_list$InitialValues) == 0) {
      input_data_list$InitialValues[[1]] <- "None"
      assign("input_data_list", input_data_list, envir = .GlobalEnv)
    }
    
    counter_advanced(counter_advanced() + 1)
    
    # =========================================================================
    # SAVE TIMESTAMPED INPUT FOLDER (mirrors wizard behaviour)
    # =========================================================================
    # All instances share a single Input_<timestamp> folder (created on the
    # first call and reused thereafter).  Each file is suffixed with the
    # current instance index so it never overwrites a sibling instance's file,
    # e.g.  PhysicalEnvironmentData_CMAQ_1.csv, ..._2.csv, etc.

    # Determine current instance index from the global instances vector
    current_idx <- if (exists("instances", envir = .GlobalEnv)) {
      tail(as.integer(get("instances", envir = .GlobalEnv)), 1)
    } else {
      counter_advanced()   # fallback
    }

    # Reuse or create the shared input folder
    # Get per-instance input dir from instance_dirs (set by main_app after summary modal)
    instance_dirs <- if (exists("instance_dirs", envir = .GlobalEnv))
      get("instance_dirs", envir = .GlobalEnv) else list()
    output_dir <- instance_dirs[[as.character(current_idx)]]$input %||%
      paste0("Input_tmp_", current_idx, "_", format(Sys.time(), "%Y-%m-%d_%H%M%S"))
    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

    # Helper: path inside the per-instance output_dir (no suffix needed)
    inp_path <- function(filename) {
      file.path(output_dir, filename)
    }

    files_written <- character(0)
    files_skipped <- character(0)
    folder_ok <- tryCatch({
      if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
      TRUE
    }, error = function(e) {
      cat("WARNING: Could not create input snapshot folder:", conditionMessage(e), "\n")
      FALSE
    })

    if (folder_ok) {
      cat("Saving input snapshot to:", output_dir, "(instance", current_idx, ")\n")

      for (i in seq_along(titles_advanced)) {
        df <- data_to_save[[i]]
        if (is.null(df)) next

        # ---- Determine output filename (with instance suffix) -----------------
        orig_path <- file_paths_advanced[i]
        base_name <- if (!is.null(orig_path) && orig_path != "none" && nzchar(orig_path)) {
          basename(orig_path)
        } else {
          paste0(gsub("[^A-Za-z0-9._-]", "_", titles_advanced[i]), ".csv")
        }
        file_name <- base_name   # no suffix — each instance has its own folder
        out_path <- file.path(output_dir, file_name)

        # ---- Write CSV or XLSX -----------------------------------------------
        tryCatch({
          if (grepl("\\.xlsx$", file_name, ignore.case = TRUE)) {
            # Multi-sheet workbook: df is a named list of data frames
            if (is.list(df) && !is.data.frame(df)) {
              wb <- openxlsx::createWorkbook()
              for (sheet_name in names(df)) {
                openxlsx::addWorksheet(wb, sheet_name)
                openxlsx::writeData(wb, sheet = sheet_name, x = df[[sheet_name]])
              }
              openxlsx::saveWorkbook(wb, out_path, overwrite = TRUE)
            } else {
              # Single data frame stored for an xlsx slot — write as xlsx
              wb <- openxlsx::createWorkbook()
              openxlsx::addWorksheet(wb, "Sheet1")
              openxlsx::writeData(wb, sheet = "Sheet1", x = df)
              openxlsx::saveWorkbook(wb, out_path, overwrite = TRUE)
            }
          } else {
            # CSV (the common case): write without row names
            write.csv(df, out_path, row.names = FALSE)
          }
          files_written <- c(files_written, file_name)
          cat("  Written:", file_name, "\n")
        }, error = function(e) {
          files_skipped <- c(files_skipped, file_name)
          cat("  WARNING: Could not write", file_name, "—", conditionMessage(e), "\n")
        })
      }

      cat("Input snapshot complete:", length(files_written), "file(s) written to", output_dir, "\n")
    }

    # =========================================================================
    # SUCCESS MODAL
    # =========================================================================
    folder_line <- if (folder_ok) {
      paste0(
        "<hr>",
        "<p style='font-size: 12px; color: #155724; background-color: #d4edda;",
        " padding: 8px; border-radius: 4px;'>",
        "📁 Input snapshot saved to: <strong>", output_dir, "</strong>",
        " (", length(files_written), " file(s))",
        if (length(files_skipped) > 0)
          paste0("<br>⚠️ Could not write: ", paste(files_skipped, collapse = ", "))
        else "",
        "</p>"
      )
    } else {
      "<hr><p style='color: orange; font-size: 12px;'>⚠️ Input snapshot folder could not be created.</p>"
    }

    showModal(modalDialog(
      title = "✅ Configuration Saved Successfully",
      HTML(paste0(
        "<p style='color: green; font-weight: bold;'>Advanced simulation configuration saved successfully!</p>",
        "<p>All input files (including any manual edits) passed final validation.</p>",
        "<p><strong>Ready to run simulation.</strong></p>",
        "<hr>",
        "<p style='font-size: 12px; color: #666;'>Configuration has been saved to global environment as 'input_data_list'.</p>",
        folder_line
      )),
      easyClose = TRUE, footer = modalButton("OK")
    ))
    modal_toggle("advanced_outputs_menu", "hide")

    # ── Initialize OutputList entry for this instance ──────────────────────────
    # compact_for_run indexes OutputList by Simulation_No (orig_idx). If the
    # entry is missing it throws "subscript out of bounds". Ensure it exists.
    current_idx_ol <- if (exists("instances", envir = .GlobalEnv))
      tail(as.integer(get("instances", envir = .GlobalEnv)), 1)
    else counter_advanced()
    if (!exists("OutputList", envir = .GlobalEnv))
      assign("OutputList", list(), envir = .GlobalEnv)
    ol <- get("OutputList", envir = .GlobalEnv)
    if (length(ol) < current_idx_ol) length(ol) <- current_idx_ol
    if (is.null(ol[[current_idx_ol]])) ol[[current_idx_ol]] <- list()
    assign("OutputList", ol, envir = .GlobalEnv)

    # ── Derive metadata from uploaded input files ──────────────────────────────
    adv_duration <- tryCatch({
      idl <- get("input_data_list", envir = .GlobalEnv)
      time_df <- idl$Time[[current_idx_ol]]
      if (!is.null(time_df) && "Duration" %in% names(time_df))
        as.numeric(time_df$Duration[1])
      else NA
    }, error = function(e) NA)

    # Read lat/lon from BoxData (always present in advanced uploads)
    adv_lat <- tryCatch({
      bd <- get("input_data_list", envir = .GlobalEnv)$BoxData[[current_idx_ol]]
      if (!is.null(bd) && "Latitude"  %in% names(bd)) as.numeric(bd$Latitude[1])  else NA
    }, error = function(e) NA)
    adv_lon <- tryCatch({
      bd <- get("input_data_list", envir = .GlobalEnv)$BoxData[[current_idx_ol]]
      if (!is.null(bd) && "Longitude" %in% names(bd)) as.numeric(bd$Longitude[1]) else NA
    }, error = function(e) NA)

    assign("pending_sim_metadata", list(
      lat       = adv_lat,
      lon       = adv_lon,
      duration  = adv_duration,
      mechanism = input$adv_mechanism %||% "SAPRC99"
    ), envir = .GlobalEnv)

    # Signal main_app that advanced config is done — main_app opens the
    # Simulation Summary modal to collect run name and metadata.
    shinyjs::runjs("Shiny.setInputValue('advanced_confirmed_internal', Math.random(), {priority: 'event'})")
  }
  
  # ---- Preload all defaults ----
  observeEvent(input$preload_all_files, {
    loaded_count <- 0
    failed_files <- character(0)
    
    for (i in seq_along(file_paths_advanced)) {
      if (file_paths_advanced[i] != "none") {
        df <- read_data(file_paths_advanced[i], show_error_modal = FALSE)
        if (!is.null(df)) {
          data_list_advanced[[paste0("data_preload", i)]] <- df
          loaded_count <- loaded_count + 1
        } else {
          failed_files <- c(failed_files, titles_advanced[i])
        }
      } else {
        data_list_advanced[[paste0("data_preload", i)]] <- data.frame(Column1 = numeric(0), Column2 = numeric(0))
        loaded_count <- loaded_count + 1
      }
    }
    
    if (length(failed_files) == 0) {
      showModal(modalDialog(
        title = "Preload Complete",
        HTML(paste0("<p style='color: green;'>Successfully preloaded all ", loaded_count, " default input files.</p>")),
        easyClose = TRUE, footer = modalButton("OK")
      ))
    } else if (length(failed_files) == length(file_paths_advanced)) {
      showModal(modalDialog(
        title = "Preload Failed",
        HTML("<p style='color: red;'>Failed to load any files. Please check that the Input directory exists and contains the required files.</p>"),
        easyClose = TRUE, footer = modalButton("OK")
      ))
    } else {
      showModal(modalDialog(
        title = "Preload Partially Complete",
        HTML(paste0(
          "<p style='color: orange;'>Loaded ", loaded_count, " out of ", length(file_paths_advanced), " files.</p>",
          "<p style='color: red;'><strong>Failed to load:</strong></p><ul>",
          paste0("<li>", failed_files, "</li>", collapse = ""),
          "</ul><p>Please check that these files exist in the Input directory.</p>"
        )),
        easyClose = TRUE, footer = modalButton("OK")
      ))
    }
  })
  
  # ---- Wire the shared output server module ----
  output_server("output_module_advanced")
}