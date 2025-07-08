library(tidyverse)
find_GUID <- function(guid = "E1C10060-DB3F-40F4-9B70-96C0096A73B4"){
  guiddf <-
    purrr::map_df(seq_along(names(NGPN_tables)), function(x){
      view_name <- names(NGPN_tables)[x]
      #print(view_name)
      view <- get(view_name, NGPN_tables)
      cols <- names(view)
      df1 <- view |> filter(if_any(everything(), ~grepl(guid, .)))
      colname <- names(df1[grepl(guid, df1)])
      #if(length(colname) > 1){rbind(colname)} else {colname}
  df2 <- df1 |>
    mutate(view = view_name,
           field = paste0(colname, collapse = ", "),
           num_rows = n()) |>
    select(view, field, num_rows) |> unique() |> separate_rows(field, sep = ", ")
})
return(guiddf)
}

find_GUID("E1C10060-DB3F-40F4-9B70-96C0096A73B4") #project$project_program_guid

#head(NGPN_tables$MonitoringStatus)
#vegcomNGPN::importData(keep_tables = T)
#library(tidyverse)
find_GUID(guid = "70912BF3-6E09-4F90-BA93-22D49167EA4F") #project$project_guid

find_GUID(guid = "70912BF3-6E09-4F90-BA93-22D49167EA4F") #mm_Project_protocol$MM_Project_GUID

find_GUID(guid = "70912BF3-6E09-4F90-BA93-22D49167EA4F") #project$Project_GUID
