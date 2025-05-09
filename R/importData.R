#' @title importData
#'
#' @description Imports data from FFI SQL Server database or csvs of FFI database tables using the schmema designed for
#' the Northern Great Plains Network plant community monitoring protocol. Currently can only import from a local installation
#' of a park FFI database in SQL Server Management Studio (SSMS), but the goal is to add an option for importing directly
#' from the SQL Server where the production FFI databases are housed. If multiple parks worth of data are imported, tables
#' are row binded, so that all NPGN parks can be queried, summarized, etc. at once.
#'
#' @importFrom dplyr collect rename tbl
#'
#' @param type Indicate how to import the database tables
#'\describe{
#'\item{"local"} Import tables in 'dbo' schema from the local installation of an FFI database in SQL Server Management Studio (SSMS).
#'\item{"server"} Import tables in 'dbo' schema from the FFI database on the production SQL Server (not enabled).
#'\item{"csv"} Import csvs that were exported from the FFI database. Option doesn't require SSMS to be installed.
#'\item{"zip"} Import zip file containing csvs that were exported from the FFI database. Option doesn't require SSMS to be installed.
#'}
#'
#' @param server If type = 'server', requires quoted FFI server address (not currently enabled).
#'
#' @param dbname If type = "server" or "local", quoted name of database matching the name of the database (eg. "FFI_RA_AGFO")
#'
#' @param path If type = "csv" or "zip", specify path of stores
#'
#'
#' @export

importData <- function(type = "local", server = NA, dbname = "FFI_RA_AGFO"){
  #---- Bug Handling ----
  # Check that suggested package required for this function are installed
  if(!requireNamespace("odbc", quietly = TRUE)){
    stop("Package 'odbc' needed for this function to work. Please install it.", call. = FALSE)}
  if(!requireNamespace("DBI", quietly = TRUE)){
    stop("Package 'DBI' needed for this function to work. Please install it.", call. = FALSE)}
  if(!requireNamespace("dbplyr", quietly = TRUE)){
    stop("Package 'dbplyr' needed for this function to work. Please install it.", call. = FALSE)}
  type <- match.arg(type, c("local", "server", "csv"))

  if(type == "server")(stop("type = 'server' is not currently enabled."))

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

  con <- odbc::dbConnect(odbc::odbc(),
                         Driver = "ODBC Driver 17 for SQL Server",
                         Server = "localhost\\SQLEXPRESS",
                         Database = dbname,
                         Trusted_Connection = "Yes"
  )

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
  }

  if(type == 'csv'){

  }

  if(type == "zip"){

  }
}
