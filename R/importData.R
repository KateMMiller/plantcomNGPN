#' @title importData
#'
#' @description Imports data from FFI SQL Server database or csvs of FFI database tables using the schmema designed for
#' the Northern Great Plains Network plant community monitoring protocol. Currently can only import from a local installation
#' of a park FFI database in SQL Server Management Studio (SSMS), but the goal is to add an option for importing directly
#' from the SQL Server where the production FFI databases are housed. If multiple parks worth of data are imported, tables
#' are row binded, so that all NPGN parks can be queried, summarized, etc. at once.
#'
#' @importFrom dplyr bind_rows collect mutate rename tbl
#' @importFrom purrr flatten map
#'
#' @param type Indicate how to import the database tables
#' \describe{
#' \item{"local"}{Import tables in 'dbo' schema from the local installation of an FFI database in SQL Server Management Studio (SSMS).}
#' \item{"server"}{Import tables in 'dbo' schema from the FFI database on the production SQL Server (not enabled).}
#' \item{"csv"}{Import csvs that were exported from the FFI database. Option doesn't require SSMS to be installed (not yet enabled).}
#' \item{"zip"}{ Import zip file containing csvs that were exported from the FFI database. Option doesn't require SSMS to be installed (not yet enabled).}
#' }
#'
#' @param server If type = 'server', requires quoted FFI server address (not currently enabled).
#'
#' @param dbname If type = "server" or "local", quoted name of database matching the name of the database (eg. "FFI_RA_AGFO"). If
#' multiple database names are specified, views will be row bound for tables in common for all of the databases with a column
#' indicating the dbname in each table. Note that the tables being row binded must be identical for this to work, and there
#' aren't thorough checks built in the function to ensure that's true (i.e., it's likely to fail, but the error message may be weird).
#'
#' @param path If type = "csv" or "zip", specify path of stores
#'
#' @param new_env Logical. If TRUE (default), will import tables to VIEWS_NGPN environment. If FALSE, will import tables to global
#' environment.
#'
#' @examples
#' \dontrun{
#'
#' # Import data for AGFO
#' importData(type = 'local', dbname = c("FFI_RA_AGFO"))
#'
#' # Import data for all NGPN parks (takes a few seconds)
#' importData(type = 'local',
#'            dbname = c("FFI_RA_AGFO", "FFI_RA_BADL", "FFI_RA_DETO", "FFI_RA_FOLA",
#'                       "FFI_RA_FOUS", "FFI_RA_JECA", "FFI_RA_KNRI", "FFI_RA_MNRR",
#'                       "FFI_RA_MORU", "FFI_RA_SCBL", "FFI_RA_THRO", "FFI_RA_WICA"))
#'
#' # Check that the multiple-park import worked
#' table(VIEWS_NGPN$MacroPlot$dbname)
#'
#' }
#'
#'
#' @export

importData <- function(type = "local", server = NA, dbname = "FFI_RA_AGFO", new_env = T){
  #---- Bug Handling ----
  # Check that suggested package required for this function are installed
  if(!requireNamespace("odbc", quietly = TRUE)){
    stop("Package 'odbc' needed for this function to work. Please install it.", call. = FALSE)}
  if(!requireNamespace("DBI", quietly = TRUE)){
    stop("Package 'DBI' needed for this function to work. Please install it.", call. = FALSE)}
  if(!requireNamespace("dbplyr", quietly = TRUE)){
    stop("Package 'dbplyr' needed for this function to work. Please install it.", call. = FALSE)}
  type <- match.arg(type, c("local", "server", "csv", 'zip'))
  stopifnot(is.logical(new_env))

  #++++++ Update as more features are added ++++++
  if(type %in% c("server", "csv", "zip")){stop(paste0("Sorry, type = ", type, " is not yet enabled."))}



  csv_list <- c("AuxSpecies", "CandidatePlot", "Cover_Points_metric_Attribute", "Cover_Points_metric_Sample",
                "Cover_SpeciesComposition_metric_Attribute", "Cover_SpeciesComposition_metric_Sample", "DataGridViewSettings",
                "Density_Belts_metric_Attribute", "Density_Belts_metric_Sample", "Density_Quadrats_metric_Attribute",
                "Density_Quadrats_metric_Sample", "DisturbanceHistory_Attribute", "DisturbanceHistory_Sample",
                "FuelConstants_CWD", "FuelConstants_DL", "FuelConstants_ExpDL", "FuelConstants_FWD", "FuelConstants_Veg",
                "Last_Modified_Date", "LocalSpecies", "LU_Contact", "LU_DataLevel", "LU_DataType", "LU_LifeCycle",
                "LU_LifeForm", "LU_MacroPlot_Type", "LU_Shape", "LU_Unit", "MacroPlot", "MasterSpecies",
                "MasterSpecies_LastModified", "MetaData", "Method", "MethodAttribute", "MethodAttributeCode", "MethodVersion",
                "MM_LocalSpecies_SpeciesPickList", "MM_Method_Reference", "MM_MonitoringStatus_SampleEvent", "MM_Organization_Method",
                "MM_Project_Protocol", "MM_ProjectUnit_MacroPlot", "MM_Protocol_Method",  "MM_SampleEvent_Protocol", "MonitoringStatus",
                "MSchange_tracking_history", "Organization", "OrganizationGroup", "PostBurnSeverity_metric_Attribute",
                "PostBurnSeverity_metric_Sample", "Program", "Project", "ProjectUnit", "Protocol", "ProtocolVersion",
                "Reference_Book", "Reference_Journal", "Reference_WebSite", "RegistrationUnit", "SampleAttribute",
                "SampleAttributeCode", "SampleEvent", "Schema_Version", "SchemaVersions", "Settings", "SpeciesPickList",
                "SurfaceFuels_1000Hr_Attribute", "SurfaceFuels_1000Hr_Sample", "SurfaceFuels_Duff_Litter_Attribute",
                "SurfaceFuels_Duff_Litter_Sample", "SurfaceFuels_Fine_Attribute", "SurfaceFuels_Fine_Sample", "sysdiagrams",
                "Trees_Individuals_metric_Attribute", "Trees_Individuals_metric_Sample")

  if(type == "local"){
  error_mess <- paste0("Unable to connect to specified SQL database. Make sure you have a local installation of the database in SSMS, ",
                       "and check that the database name is correct.")

  if(length(dbname) == 1){
    tryCatch(
      con <- odbc::dbConnect(odbc::odbc(),
                             Driver = "ODBC Driver 17 for SQL Server",
                             Server = "localhost\\SQLEXPRESS",
                             Database = dbname,
                             Trusted_Connection = "Yes"),
     error = function(e){stop(error_mess)},
     warning = function(w){stop(error_mess)})

  tbls <<- DBI::dbListTables(con, schema = "dbo")

  # Setup progress bar
  pb <- txtProgressBar(min = 0, max = length(tbls), style = 3)

  # Import views using their names and show progress bar
  tbl_import <- lapply(seq_along(tbls), function(x){
    setTxtProgressBar(pb, x)
    tbl <- tbls[x]
    tab <- dplyr::tbl(con, dbplyr::in_schema("dbo", tbl)) |> dplyr::collect() |>
      as.data.frame()
    return(tab)})

  tbl_import <- setNames(tbl_import, tbls)
  VIEWS_NGPN <<- new.env()
  list2env(tbl_import, envir = VIEWS_NGPN)
  DBI::dbDisconnect(con)


  # Add check that all csvs were imported
    } else if(length(dbname) > 1){

    dbimport <-
      purrr::map(seq_along(dbname), function(db){
        error_mess = paste0("Unable to connect to specified SQL database named ", dbname[db],
                            ". Make sure you have a local installation of the database in SSMS, ",
                            "and check that the database name is correct.")

        tryCatch(
          con <- odbc::dbConnect(odbc::odbc(),
                                 Driver = "ODBC Driver 17 for SQL Server",
                                 Server = "localhost\\SQLEXPRESS",
                                 Database = dbname[db],
                                 Trusted_Connection = "Yes"),
          error = function(e){stop(error_mess)},
          warning = function(w){stop(error_mess)})

        tbls <<- DBI::dbListTables(con, schema = "dbo")

      # Import views using their names and show progress bar
      tbl_import <- lapply(seq_along(tbls), function(x){
       # setTxtProgressBar(pb, x)
        tbl <- tbls[x]
        tab <- dplyr::tbl(con, dbplyr::in_schema("dbo", tbl)) |> dplyr::collect() |>
          as.data.frame() |> mutate(dbname = dbname[db])
        return(tab)})
      DBI::dbDisconnect(con)
      tbl_import <- setNames(tbl_import, tbls)
      #list2env(tbl_import)
      }, .progress = T) |> purrr::set_names(dbname)

    dbflat1 <- flatten(dbimport)
    dbflat2 <- tapply(dbflat1, names(dbflat1),dplyr::bind_rows)

    VIEWS_NGPN <<- new.env()
    list2env(dbflat2, envir = VIEWS_NGPN)

  } # end of dbname>1
  } # end of db=local

  if(type == 'csv'){

  }

  if(type == "zip"){

  }
}
