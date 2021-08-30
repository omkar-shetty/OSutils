
#' Title Convert Date to a numeric value
#'
#' @param date Vector of dates to be converted to integers
#'
#' @return
#' @export
#'
#' @examples convert_date_to_idnt(Sys.Date())
convert_date_to_idnt <- function(date){

  dt = as.Date(date)
  ref_year = year(dt)
  ref_dt = as.Date(paste0(ref_year,'-01-01'))
  delta = as.numeric(difftime(dt,ref_dt, unit = "days"))
  delta = stringr::str_pad(delta, width = 3, side = 'left', pad = '0')
  idnt = as.numeric(paste0(ref_year, delta))

  return(idnt)
}
