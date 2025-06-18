#' @title importData
#'
#' @description Imports data from FFI SQL Server database or csvs of FFI database tables using the schema designed for
#' the Northern Great Plains Network plant community monitoring protocol. Currently can only import from a local installation
#' of a park FFI database in SQL Server Management Studio (SSMS), but the goal is to add an option for importing directly
#' from the SQL Server where the production FFI databases are housed. If multiple parks worth of data are imported, tables
#' are row binded, so that all NPGN parks can be queried, summarized, etc. at once.
#'
#' @importFrom dplyr bind_rows collect mutate rename tbl
#' @importFrom purrr flatten map set_names
#'
#' @param type Indicate how to import the database tables
#' \describe{
#' \item{"local"}{Import tables in 'dbo' schema from the local installation of an FFI database in SQL Server Management Studio (SSMS).}
#' \item{"server"}{Import tables in 'dbo' schema from the FFI database on the production SQL Server (not enabled).}
#' \item{"csv"}{ Import zip file containing csvs that were exported from the FFI database. Option doesn't require SSMS to be installed (not yet enabled).}
#' }
#'
#' @param server If type = 'server', requires quoted FFI server address (not currently enabled).
#'
#' @param dbname If type = "server" or "local", quoted name of database matching the name of the database (eg. "FFI_RA_AGFO"). If
#' multiple database names are specified, views will be row bound for tables in common for all of the databases with a column
#' indicating the dbname in each table. Note that the tables being row binded must be identical for this to work, and there
#' aren't thorough checks built in the function to ensure that's true (i.e., it's likely to fail, but the error message may be weird).
#'
#' @param new_env Logical. If TRUE (default), will import tables to NGPN_tables environment. If FALSE, will import tables to global
#' environment.
#'
#' @param export Logical. If TRUE, will export a zip file of csvs to specified export_path.
#'
#' @param export_path Quoted string to export zipped csvs to if export = TRUE. If not specified, will export to the working directory.
#'
#' @param import_path Quoted string to import a zipped file of csvs if type = 'csv'. The name of the zipped file should be included
#' in the path. Can specify multiple paths to import multiple parks/projects.
#'
#' @examples
#' \dontrun{
#' #--- From Local install of FFI SQL databases
#' # Import data for AGFO and export tables to zip file
#' importData(type = 'local', dbname = c("FFI_RA_AGFO"), export = T)
#'
#' # Import data for all NGPN parks (takes a few seconds) from local copy on SSMS
#' importData(type = 'local',
#'            dbname = c("FFI_RA_AGFO", "FFI_RA_BADL", "FFI_RA_DETO", "FFI_RA_FOLA",
#'                       "FFI_RA_FOUS", "FFI_RA_JECA", "FFI_RA_KNRI", "FFI_RA_MNRR",
#'                       "FFI_RA_MORU", "FFI_RA_SCBL", "FFI_RA_THRO", "FFI_RA_WICA"),
#'            export = T,
#'            export_path = "C:/temp")
#'
#' # Check that the multiple-park import worked
#' table(NGPN_tables$MacroPlot$datasource)
#'
#' #--- From zipped csvs of FFI data
#' # Import THRO from zip file
#' importData(type = 'csv', import_path = "C:/temp/FFI_RA_THRO.zip")
#'
#' # Import zipped files of NGPN parks and export as 1 zip file with all parks included
#' zips <- c("NGPN_FFI_tables_AGFO_20250508.zip", "NGPN_FFI_tables_BADL_20250508.zip", "NGPN_FFI_tables_DETO_20250508.zip",
#'           "NGPN_FFI_tables_FOLA_20250508.zip", "NGPN_FFI_tables_FOUS_20250508.zip", "NGPN_FFI_tables_JECA_20250508.zip",
#'           "NGPN_FFI_tables_KNRI_20250508.zip", "NGPN_FFI_tables_MORU_20250508.zip", "NGPN_FFI_tables_SCBL_20250508.zip",
#'           "NGPN_FFI_tables_THRO_20250508.zip", "NGPN_FFI_tables_WICA_20250508.zip")
#' filepath = "C:/Users/KMMiller/OneDrive - DOI/MWR/NGPN_veg/FFI_zips/"
#' zips_full <- paste0(filepath, zips)
#' importData(type = 'csv', import_path = zips_full, export = T)
#'
#' }
#'
#' @returns Either an environment with database tables as data frames for each imported database, or database
#' tables directly in the global environment.
#'
#' @export
#'

importData <- function(type = "local", server = NA, dbname = "FFI_RA_AGFO", new_env = T, export = F,
                       export_path = NA, import_path = NA){
  #---- Bug Handling ----
  # Check that suggested package required for this function are installed
  # Need to make this conditional on type.
  if(type %in% c("local", "server") & !requireNamespace("odbc", quietly = TRUE)){
    stop("Package 'odbc' needed for this function to work. Please install it.", call. = FALSE)}
  if(type %in% c("local", "server") & !requireNamespace("DBI", quietly = TRUE)){
    stop("Package 'DBI' needed for this function to work. Please install it.", call. = FALSE)}
  if(type %in% c("local", "server") & !requireNamespace("dbplyr", quietly = TRUE)){
    stop("Package 'dbplyr' needed for this function to work. Please install it.", call. = FALSE)}
  if(!requireNamespace("zip", quietly = TRUE) & export == T){
    stop("Package 'zip' needed when export = TRUE. Please install it.", call. = FALSE)}
  type <- match.arg(type, c("local", "server", 'csv'))
  stopifnot(is.logical(new_env))
  stopifnot(is.logical(export))
  if(any(type %in% c("local", "server")) & any(is.na(dbname))){stop("Must specify a dbname if type is 'local' or 'server'")}
  if(type == "server" & is.na(server)){stop("Must specify a server address if type = 'server'")}

  #++++++ Update as more features are added ++++++
  if(type %in% c("server")){stop(paste0("Sorry, type = ", type, " is not yet enabled."))}

  # Error handling for paths
  if(export == TRUE){
    if(is.na(export_path)){
      export_path <- getwd()
      print(paste0("No export_path specified. Output saved to working directory: ", getwd()), quote = FALSE)}
    if(!grepl("/$", export_path)){export_path <- paste0(export_path, "/")} # add / to end of path if doesn't exist
    if(!dir.exists(export_path)){stop("Specified export_path directory does not exist.")}
    # Normalize filepath for zip
    export_pathn <- normalizePath(export_path)
    }

  if(type == 'csv'){
    if(!any(file.exists(import_path))){
      stop(paste0("Specified import_path does not exist. ",
                  ifelse(any(grepl("sharepoint", import_path)), " Note that file paths from Sharepoint or Teams are not accessible.",
                         "")))}

    if(all(is.na(import_path))){stop("Must specify an import_path for type = 'csv'.")}
    if(any(!grepl(".zip$", import_path))){stop("Must include the name of the zip file in import_path.")} # add / to end of path if doesn't exist
    if(any(!file.exists(import_path))){stop("Specified import_path directory does not exist.")}
    # Normalize filepath for zip
    import_pathn <- normalizePath(import_path)
  }


  if(new_env == TRUE){NGPN_tables <<- new.env()}
  env <- if(new_env == TRUE){NGPN_tables} else {.GlobalEnv}

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

  tbls <- DBI::dbListTables(con, schema = "dbo")

  # Setup progress bar
  pb <- txtProgressBar(min = 0, max = length(tbls), style = 3)

  # Import views using their names and show progress bar
  tbl_import <- lapply(seq_along(tbls), function(x){
    setTxtProgressBar(pb, x)
    tbl <- tbls[x]
    tab <- dplyr::tbl(con, dbplyr::in_schema("dbo", tbl)) |> dplyr::collect() |>
      as.data.frame() |> dplyr::mutate(datasource = dbname)
    return(tab)})

  tbl_import <- setNames(tbl_import, tbls)
  tbl_import2 <- tbl_import[sort(names(tbl_import))]
  # remove empty tables
  tbl_import3 <- tbl_import2[sapply(tbl_import2, nrow) > 0]

  list2env(tbl_import3, envir = env)
  DBI::dbDisconnect(con)

  if(export == TRUE){
    dir.create(tmp <- tempfile())
    dbtbls <- names(tbl_import3)

    invisible(lapply(seq_along(dbtbls), function(x){
      temp_tbl = get(dbtbls[x], envir = env)
      write.csv(temp_tbl,
                paste0(tmp, "\\", dbtbls[x], ".csv"),
                row.names = FALSE)
    }))

    file_list <- list.files(tmp)
    park <- substr(dbname, nchar(dbname)-3, nchar(dbname))
    zip_name = paste0("NGPN_FFI_export_", park, "_", format(Sys.Date(), "%Y%m%d"), ".zip")

    zip::zipr(zipfile = paste0(export_pathn, "\\", zip_name),
              root = tmp,
              files = file_list)

  }

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

        tbls <- DBI::dbListTables(con, schema = "dbo")

      # Import views using their names and show progress bar
      tbl_import <- lapply(seq_along(tbls), function(x){
       # setTxtProgressBar(pb, x)
        tbl <- tbls[x]
        tab <- dplyr::tbl(con, dbplyr::in_schema("dbo", tbl)) |> dplyr::collect() |>
          as.data.frame() |> dplyr::mutate(datasource = dbname[db])
        return(tab)})
      DBI::dbDisconnect(con)
      tbl_import <- setNames(tbl_import, tbls)
      #list2env(tbl_import)
      }, .progress = T) |> purrr::set_names(dbname)

    # flatten list to bind like tables together
    dbflat1 <- purrr::flatten(dbimport)
    dbflat2 <- tapply(dbflat1, names(dbflat1), dplyr::bind_rows)
    # remove empty tables
    dbflat3 <- dbflat2[sapply(dbflat2, nrow) > 0]
    # sort tables alphabetically
    dbflat4 <- dbflat3[sort(names(dbflat3))]

    list2env(dbflat4, envir = env)

    if(export == TRUE){
      dir.create(tmp <- tempfile())
      dbtbls <- names(dbflat4)

      invisible(lapply(seq_along(dbtbls), function(x){
        temp_tbl = get(dbtbls[x], envir = env)
        write.csv(temp_tbl,
                  paste0(tmp, "\\", dbtbls[x], ".csv"),
                  row.names = FALSE)
      }))

      file_list <- list.files(tmp)

      zip_name = paste0("NGPN_FFI_export_", format(Sys.Date(), "%Y%m%d"), ".zip")

      zip::zipr(zipfile = paste0(export_pathn, "\\", zip_name),
                root = tmp,
                files = file_list)
    }

  } # end of dbname>1
  } # end of type = local

  if(type == 'csv'){

    # Pulling in only tables commonly used across NGPN parks
    csv_list1 <- c('AuxSpecies', 'Cover_Frequency_metric_Attribute', 'Cover_Frequency_metric_Sample',
                  'Cover_Points_metric_Attribute', 'Cover_Points_metric_Sample', 'Cover_SpeciesComposition_metric_Attribute',
                  'Cover_SpeciesComposition_metric_Sample', 'DataGridViewSettings', 'Density_Belts_metric_Attribute',
                  'Density_Belts_metric_Sample', 'Density_Quadrats_metric_Attribute', 'Density_Quadrats_metric_Sample',
                  'DisturbanceHistory_Attribute', 'DisturbanceHistory_Sample', 'FuelConstants_CWD', 'FuelConstants_DL',
                  'FuelConstants_ExpDL', 'FuelConstants_FWD', 'FuelConstants_Veg', 'LU_Contact', 'LU_DataLevel', 'LU_DataType',
                  'LU_LifeCycle', 'LU_LifeForm', 'LU_MacroPlot_Type', 'LU_Shape', 'LU_Unit', 'Last_Modified_Date', 'LocalSpecies',
                  'MM_LocalSpecies_SpeciesPickList', 'MM_Method_Reference',
                  'MM_MonitoringStatus_SampleEvent', 'MM_Organization_Method', 'MM_ProjectUnit_MacroPlot',
                  'MM_Project_Protocol', 'MM_Protocol_Method', 'MM_SampleEvent_Protocol', 'MSchange_tracking_history',
                  'MacroPlot', 'MasterSpecies', 'MasterSpecies_LastModified', 'Method', 'MethodAttribute', 'MethodAttributeCode',
                  'MethodVersion', 'MonitoringStatus', 'Organization', 'OrganizationGroup', 'PostBurnSeverity_metric_Attribute',
                  'PostBurnSeverity_metric_Sample', 'Program', 'Project', 'ProjectUnit', 'Protocol', 'ProtocolVersion', 'RegistrationUnit',
                  'SampleAttribute', 'SampleAttributeCode', 'SampleEvent', 'SchemaVersions', 'Schema_Version', 'Settings',
                  'SpeciesPickList', 'SurfaceFuels_1000Hr_Attribute', 'SurfaceFuels_1000Hr_Sample', 'SurfaceFuels_Duff_Litter_Attribute',
                  'SurfaceFuels_Duff_Litter_Sample', 'SurfaceFuels_Fine_Attribute', 'SurfaceFuels_Fine_Sample',
                  #"SurfaceFuels_Hr_Attribute", "SurfaceFuels_Hr_Sample",
                  'Trees_Individuals_metric_Attribute', 'Trees_Individuals_metric_Sample')


    if(length(import_path) == 1){

      file_name1 <- sort(sub(".*/", "", import_path, perl = T))
      file_name2 <- gsub("[[:digit:]]+|.zip", "", file_name1)
      file_name <- gsub("_$","", file_name2)

      # Check if can read files within the zip file
      tryCatch(
        {zfiles = utils::unzip(import_path, list = T)$Name},
         error = function(e){stop(paste0("Unable to import specified zip file."))})

      z_list = sort(zfiles[grepl(paste0(csv_list1, collapse = "|"), zfiles)])

      # Drop date stamp (if it exists) from file name if exists in 2 steps
      z_list_names <- gsub("[[:digit:]]+|.csv", "", z_list)
      z_list_names <- gsub("./", "", z_list_names)
      z_list_names <- gsub("_$","", z_list_names)

      # Drop csvs from csv_list not in z_list
      csv_list <- csv_list1[csv_list1 %in% z_list_names]

      miss_tbls <- setdiff(z_list_names, csv_list) # currently circular. Once I know the tables that should always be included,
      # I'll update csv_list1 above and use the same tables for each park.

      # Check for missing views
      if(length(miss_tbls) > 0){stop("Missing the following tables from the specified import_path: ",
                                     paste0(miss_tbls, collapse = ", "))}

      # Since the missing test passed, clean up files so only includes names in view_list, but
      # maintain order in files

      # Import views now that all tests passed
      pb <- txtProgressBar(min = 0, max = length(z_list), style = 3)

      tbls1 <- unzip(import_path, junkpaths = TRUE, exdir = tempdir())
      tbls2 <- sort(tbls1[grepl(".csv", tbls1)])

      tbls <- sort(tbls2[grepl(paste0(csv_list, collapse = "|"), tbls2)])

      tbl_import <-
        lapply(seq_along(tbls), function(x){
          setTxtProgressBar(pb,x)
          tbl <- tbls[x]
          tab <- read.csv(tbls[x], na.string = c("NA", "NULL"), check.names = FALSE) |>
            dplyr::mutate(datasource = file_name)
          return(tab)})

      tbl_import <- setNames(tbl_import, z_list_names)
      list2env(tbl_import, envir = env)
      # Close progress bar
      close(pb)

      if(export == TRUE){
        dir.create(tmp <- tempfile())
        csvtbls <- names(tbl_import)

        invisible(lapply(seq_along(csvtbls), function(x){
          temp_tbl = get(csvtbls[x], envir = env)
          write.csv(temp_tbl,
                    paste0(tmp, "\\", csvtbls[x], ".csv"),
                    row.names = FALSE)
        }))

        file_list <- list.files(tmp)
        park <- substr(dbname, nchar(dbname)-3, nchar(dbname))
        zip_name = paste0(file_name, "_", format(Sys.Date(), "%Y%m%d"), ".zip")

        zip::zipr(zipfile = paste0(export_pathn, "\\", zip_name),
                  root = tmp,
                  files = file_list)
        }

    } else if(length(import_path > 1)){

      file_names1 <- sort(sub(".*/", "", import_path, perl = T))
      file_names2 <- gsub("[[:digit:]]+|.zip", "", file_names1)
      file_names <- gsub("_$","", file_names2)

      zip_import <-
      purrr::map(seq_along(import_path), function(ip){

        file_name = file_names[ip]

        tryCatch(
          {zfiles = utils::unzip(import_path[ip], list = T)$Name},
           error = function(e){stop(paste0("Unable to import specified zip file: ", ip))})

        z_list = sort(zfiles[grepl(paste0(csv_list1, ".csv", collapse = "|"), zfiles)])

        # Drop date stamp (if it exists) from file name if exists in 2 steps
        #z_list_names <- gsub("[[:digit:]]+|.csv", "", z_list)
        z_list_names <- gsub("./", "", z_list)
        z_list_names <- gsub(".csv$","", z_list_names)
        z_list_names <- gsub("_$","", z_list_names)

        # Drop csvs from csv_list not in z_list
        csv_list <- csv_list1[csv_list1 %in% z_list_names]

        # "SurfaceFuels_Hr_Attribute", "SurfaceFuels_Hr_Sample",

        miss_tbls <- setdiff(z_list_names, csv_list) # currently circular. Once I know the tables that should always be included,
        # I'll update csv_list1 above and use the same tables for each park.

        # Check for missing views
        if(length(miss_tbls) > 0){stop("Missing the following tables from the specified import_path: ",
                                       import_path[ip], "\n",
                                       paste0(miss_tbls, collapse = ", "))}

        # Since the missing test passed, clean up files so only includes names in view_list, but
        # maintain order in files
        tbls1 <- unzip(import_path[ip], junkpaths = TRUE, exdir = tempdir())
        tbls2 <- sort(tbls1[grepl(".csv", tbls1)])
        tbls <- sort(tbls2[grepl(paste0(csv_list, ".csv", collapse = "|"), tbls2)])

        tbl_import <-
          lapply(seq_along(tbls), function(x){
          tbl_temp <- read.csv(tbls[x], na.strings = c("NA", "NULL", ""), check.names = FALSE, as.is = T) |>
            dplyr::mutate(datasource = file_name)

          # When fields from one park are all NAs vs other parks have data, read.csv doesn't always assign
          # them as consistent field types. Converting these to character by default. If importData returns
          # error that Can't Combine two columns, it needs to be added here. I couldn't figure out how
          # to generalize this better, given that parks have different tables.
          cols_to_char <-
              c("UV1", "UV2", "UV3", "AttributeData_DataRow_GUID", "AttributeData_SampleRow_GUID",
               "Spp_GUID", "Status", "AgeCl", "Comment", "AttributeData_Original_GUID",
               "AttributeData_CreatedBy", "AttributeData_CreatedDate", "AttributeData_ModifiedBy",
               "AttributeData_ModifiedDate", "SampleData_SampleRow_GUID", "SampleData_SampleEvent_GUID",
               "FieldTeam", "EntryTeam", "SaComment", "SampleData_Original_GUID",
               "SampleData_CreatedBy", "SampleData_CreatedDate", "SampleData_ModifiedBy",
               "SampleData_ModifiedDate", "MethodAtt_Value_Default", "ProtocolVersion_GUID",
               "ProtocolVersion_Family_GUID", "ProtocolVersion_Protocol_GUID", "ProtocolVersion_TimeStamp",
               "UV1Desc", "UV2Desc", "UV3Desc", "TypeCov", "NFRatio", "NFNum", "SizeCl",
               "MacroPlot_UV5", "MacroPlot_UV7", "MM_SpeciesPickList_GUID", "MM_LocalSpecies_GUID",
               "MonitoringStatus_Suffix", "SampleAttributeCode_GUID", "SampleAttributeCode_SampleAttribute_GUID",
               "SampleAttributeCode_Code", "SampleAttributeCode_Text", "SampleAttributeCode_Description",
               "val1", "key1", "SpeciesPickList_GUID", "SpeciesPickList_RegistrationUnitGUID",
               "SpeciesPickList_Name", "SpeciesPickList_Describe", "DamCd3", "DamCd4", "DamCd5")

          if(any(cols_to_char %in% names(tbl_temp))){
            cols_to_char2 <- cols_to_char[cols_to_char %in% names(tbl_temp)]
            tbl_temp[,cols_to_char2] <- sapply(tbl_temp[,cols_to_char2], as.character)
            #tbl_temp[,cols_to_char2] <- as.character(tbl_temp[,cols_to_char2])
            }

          return(tbl_temp)
          })

        tbl_import <- setNames(tbl_import, z_list_names)},
        .progress = TRUE) |> purrr::set_names(file_names)

      # flatten list to bind like tables together
      zipflat1 <- purrr::flatten(zip_import)
      zipflat2 <- tapply(zipflat1, names(zipflat1), dplyr::bind_rows)
      # remove empty tables
      zipflat3 <- zipflat2[sapply(zipflat2, nrow) > 0]
      # sort tables alphabetically
      zipflat4 <- zipflat3[sort(names(zipflat3))]

      list2env(zipflat4, envir = env)

      if(export == TRUE){
        dir.create(tmp <- tempfile())
        ziptbls <- names(zipflat4)

        invisible(lapply(seq_along(ziptbls), function(x){
          temp_tbl = get(ziptbls[x], envir = env)
          write.csv(temp_tbl,
                    paste0(tmp, "\\", ziptbls[x], ".csv"),
                    row.names = FALSE)
        }))

        file_list <- list.files(tmp)

        zip_name = paste0("NGPN_FFI_export_", format(Sys.Date(), "%Y%m%d"), ".zip")

        zip::zipr(zipfile = paste0(export_pathn, "\\", zip_name),
                  root = tmp,
                  files = file_list)}
      } # end of fp 2
      } # type = csv
  if(export == TRUE){noquote(paste0('Export complete. Data package saved to: ', export_pathn, "\\", zip_name))}

  } # end of function

