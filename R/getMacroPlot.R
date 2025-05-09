#' @title getMacroPlot
#'
#' @description This function filters macroplot data by plot name and purpose from FFI data.
#'
#' @importFrom dplyr filter select
#'
#' @param plot_name Quoted string to return a particular plot based on name. Default is "all".
#' Can select multiple plots.
#'
#' @param purpose Quoted string to return plots with a particular purpose. Note that purpose
#' is not always standardized. For example "Panel9" and "Panel 9" are both options for different plots.
#'
#' @param output Quoted string. Options are "short" (default), which only returns most important columns.
#' "verbose" returns all columns in the MacroPlot database table.
#'
#' @examples
#' \dontrun{
#'
#' #+++ ADD EXAMPELS +++
#'
#' }
#'
#' @return Returns a data frame of macroplots
#'
#' @export

getMacroPlot <- function(plot_name = "all", purpose = "all"){

  #---- Bug handling ----
  # Can't do plot_name matching. Will at least check that specified plot names are included in the data.

    hgm_class <- match.arg(hgm_class, choices = c("all", "Depression", "Impoundment", "Riverine", "Slope"),
                         several.ok = T)
  dom_veg1 <- match.arg(dom_veg1, choices = c("all", "emergent", "forest", "shrub"), several.ok = T)
  nativity <- match.arg(nativity, choices = c("all", "adventive", "cryptogeni", "native"), several.ok = T)
  stopifnot(class(years) == "numeric" | class(years) == "integer", years >= 2008)
  stopifnot(class(intens_mods) == "numeric" | class(intens_mods) == "integer")

  #---- Compile data ----
  env <- if(exists("HTLNwetlands")){HTLNwetlands} else {.GlobalEnv}

table(macroplot$Purpose)
}
