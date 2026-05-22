#' Run the SIACS Shiny Application
#' 
#' Launches the interactive GUI for the SIACS model.
#' 
#' @export
runSIACSApp <- function() {
  siacs_app_wd <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  old_opt <- getOption("SIACS.app_wd")
  options("SIACS.app_wd" = siacs_app_wd)
  on.exit(options("SIACS.app_wd" = old_opt), add = TRUE)

  appDir <- system.file("shiny/siacs_app", package = "SIACS")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `SIACS`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}
