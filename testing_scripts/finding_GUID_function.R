
find_GUID <- function(guid = "E1C10060-DB3F-40F4-9B70-96C0096A73B4"){
  guiddf <-
    purrr::map_df(seq_along(names(VIEWS_NGPN)), function(x){
      view_name <- names(VIEWS_NGPN)[x]
      #print(view_name)
      view <- get(view_name, VIEWS_NGPN)
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


