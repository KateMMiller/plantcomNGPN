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

head(NGPN_tables$MonitoringStatus)
vegcomNGPN::importData()
#library(tidyverse)
find_GUID(guid = "0BE4D02B-D61B-4A17-878A-964239FA3E18")
find_GUID(guid = "50A07E97-8EAA-4ABD-8331-2C4AC1A36578")
find_GUID("B5632327-D105-40D2-8393-0505AAF0EB17")
