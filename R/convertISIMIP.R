#' @title convertISIMIP
#' @description convert data to ISO country level
#' 
#' @param x MAgPIE object on cellular level
#' @param subtype data subtype 
#' @return MAgPIE object on country level
#' 
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{readSource}}
#' 
#' @examples
#' \dontrun{
#' a <- readSource("ISIMIPoutputs", convert=TRUE)
#' }
#' 

convertISIMIP <- function(x, subtype){
  if (grepl("^airww", subtype)) {
    landarea <- dimSums(calcOutput("LUH2v2", landuse_types="magpie", aggregate=FALSE, cellular=TRUE, cells="lpjcell", irrigation=FALSE, years="y1995"), dim=3)
    landarea <- addLocation(landarea)
    landarea <- collapseDim(landarea, dim=c(1.1, 1.2))
    weight <- landarea
  } else stop("Aggregation rule for given subtype \"",subtype,"\" not defined!")
  return(toolAggregateCell2Country(x, weight = weight, fill = 0))
}