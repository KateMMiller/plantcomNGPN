#' @title importViews
#'
#' @description This function imports flattened views of the FFI data for NGPN PCM monitoring.
#' The views were generated by the importData() function and exported as a zip file,
#' and will contain data from all parks imported via importData(). By importing a zip file of csvs,
#' this function is much faster than importing the individual tables from each FFI SQL database
#' and joining them together to create the views. This option also saves having to set up software
#' to work with SQL servers, and works seamlessly with package functions just like the
#' the importData() function.
#'
#' @param new_env Logical. If TRUE (default), will import tables to VIEWS_NGPN environment.
#' If FALSE, will import tables to global environment.
#'
#' @param import_path Quoted string to import a zipped file of views. The name of the zipped file should be included
#' in the path. Can specify multiple paths to import multiple parks/projects.
#'
#' @examples
#' \dontrun{
#' library(plantcomNGPN)
#' # import views from specified path
#' importViews(import_path = "C:/temp/NGPN_FFI_views_20250708.zip")
#'
#' }
#'
#' @returns Either an environment with views as data frames for each imported database, or database
#' tables directly in the global environment.
#'
#' @export
#'

importViews <- function(new_env = T, import_path = NA){
  #---- Bug Handling ----
  # Check that suggested package required for this function are installed
  stopifnot(is.logical(new_env))
  if(all(is.na(import_path))){stop("Must specify an import_path.")}
  if(any(!file.exists(import_path))){stop("Specified import_path directory does not exist.")}
  if(any(!grepl(".zip$", import_path))){stop("Must include the name of the zip file in import_path.")} # add / to end of path if doesn't exist
  # Normalize filepath for zip
  #import_pathn <- normalizePath(import_path)

  cat(noquote("Importing views."), "\n\n")

  env <- if(new_env == TRUE){VIEWS_NGPN <<- new.env()
  } else {.GlobalEnv}


  views <- c("Cover_Points_metric", "Cover_Species_Composition", "Density_Belts_metric",
             "Density_Quadrats_metric", "Disturbance_History", "MacroPlots", "SampleEvents",
             "Surface_Fuels_1000Hr", "Surface_Fuels_Duff", "Surface_Fuels_Fine", "Taxa_Table",
             "Trees_metric")

  # Check if can read files within the zip file
  tryCatch({
     zfiles = utils::unzip(import_path, list = T)$Name},
     error = function(e){stop(paste0("Unable to import specified zip file."))})

   # Pull in only files in zip that match view names (eg drops xml if in data package)
   z_list = sort(zfiles[grepl(paste0(views, collapse = "|"), zfiles)])
   z_list_names <- gsub(".csv", "", z_list)

   # Drop csvs from csv_list not in z_list
   view_list <- views[views %in% z_list_names]

   miss_vws <- setdiff(z_list_names, view_list)
   req_vws <- z_list_names[z_list_names %in% c("MacroPlots", "SampleEvents")]

   # Check for required views
   if(length(req_vws) < 2){
     stop("Required views are missing from the zip file. Please specify a zip file containing at least the following tables: ",
          paste("MacroPlots", "SampleEvents", sep = ", "))}

   # Warn about non-required missing views
   if(length(miss_vws) > 0){
      warning("The following views are not included in specified zip, and can't be queried by their matching function: ",
              paste0(miss_vws, collapse = ", "))}

   # Import views now that all tests passed
   pb <- txtProgressBar(min = 0, max = length(view_list), style = 3)

  vws1 <- unzip(import_path, junkpaths = TRUE, exdir = tempdir())
  vws2 <- sort(vws1[grepl(".csv", vws1)])

  vws <- sort(vws2[grepl(paste0(view_list, collapse = "|"), vws2)])

  view_import <- lapply(seq_along(vws), function(x){
    setTxtProgressBar(pb,x)
    vw <- vws[x]
    view <- read.csv(vws[x], na.string = c("NA", "NULL"), check.names = FALSE)
    return(view)})

    view_import <- setNames(view_import, z_list_names)
    list2env(view_import, envir = env)

  # Close progress bar
  close(pb)

  } # end of function
