library(odbc)
library(DBI)
library(dbplyr)

#params = (instance = c("local", "server"), server = NA, name = "FFI_RA", new_env = TRUE)
#db_name = paste0(name, "_", park)

#----- Restore bak files as database ----
# renaming bak files, so easier to deal with
# bak_list <- list.files("C:/temp", pattern = "FFI_RA_", full.names = T)
# bak_rename <- gsub(" Tuesday, May 6, 2025 0905", "_20250506", bak_list)
# file.rename(bak_list, bak_rename)

# Import function to pull restored DB tables from SSMS into R
importData <- function(instance = "local", dbname = "FFI_RA", park = "BADL"){
  db_name <- paste0(dbname, "_", park)
  con <- odbc::dbConnect(odbc::odbc(),
                 Driver = "ODBC Driver 17 for SQL Server",
                 Server = "localhost\\SQLEXPRESS",
                 Database = db_name,
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
    return(tab)
   })

  tbl_import <- setNames(tbl_import, tbls)
  VIEWS_NGPN <<- new.env()
  list2env(tbl_import, envir = VIEWS_NGPN)

  DBI::dbDisconnect(con)
}

# Exports tables to zip with .csvs
exportData <- function(park = "BADL", path = "C:/Users/KMMiller/OneDrive - DOI/MWR/NGPN_veg/zips/"){
  dir.create(tmp <- tempfile())
  pathn <- normalizePath(path)


  invisible(lapply(seq_along(tbls), function(x){
    write.csv(get(tbls[[x]], envir = VIEWS_NGPN),
              paste0(tmp, "\\", tbls[x], ".csv"),
              row.names = FALSE)}))

  file_list <- list.files(tmp)

  zip::zipr(zipfile = paste0(pathn, "\\NPGN_FFI_tables_", park, "_", format(Sys.Date(), "%Y%m%d"), ".zip"),
            root = tmp,
            files = file_list)

  noquote(paste0("Exported ", paste0(pathn, "\\NPGN_FFI_tables_", park, "_", format(Sys.Date(), "%Y%m%d"), ".zip")))

}

# Iterate through list to import/export database tables for each park.
# First you need SSMS installed/set up on machine
# Then need to restore databases from BAK. Can run ./SQL/loop_restore_database.sql in SSMS to restore all
ngpn_list <- c("AGFO", "BADL", "DETO", "FOLA", "FOUS", "JECA",
               "KNRI", "MNRR", "MORU", "SCBL", "THRO", "WICA")

purrr::map(ngpn_list, function(parkcode){
  importData(park = parkcode)
  exportData(park = parkcode)
})
