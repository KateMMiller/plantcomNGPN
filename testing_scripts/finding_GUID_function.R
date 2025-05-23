
find_GUID <- function(guid = "E1C10060-DB3F-40F4-9B70-96C0096A73B4"){
  guiddf <-
    purrr::map_dfr(seq_along(names(VIEWS_NGPN)), function(x){
      view_name <- names(VIEWS_NGPN)[x]
      view <- get(view_name, VIEWS_NGPN)
      cols <- names(view)
      df1 <- view |> filter(if_any(everything(), ~grepl(guid, .)))
      colname <- names(df1[grepl(guid, df1)])

  df2 <- df1 |>
    mutate(view = view_name,
           col = colname,
           num_rows = n()) |>
    select(view, colname, num_rows) |> unique()
})
return(guiddf)
}


find_GUID()

