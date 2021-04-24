#' IDW interpolation wrapper
#'
#' @param formula,data,locations See [gstat::idw()].
#'
#' @return `myidw` returns a list with the formula and data, i.e. an object of class `myidw`. The predict method return only the predicted values as a numeric vector.
#' @export
myidw <- function(formula, data) {
  m <- list(formula = formula, data = data)
  class(m) <- "myidw"
  m
}


#' @describeIn myidw Predict method for `myidw` objects.
#' @export
predict.myidw <- function(object, newdata, locations = ~x+y,
                          nmax = 7, ...) {
  if (nrow(newdata) == 0) return(numeric())
  res <- gstat::idw(formula = object$formula,
                    locations = locations,
                    object$data,
                    newdata = newdata, debug.level = 0,
                    nmax = nmax, ...)
  res <- res$var1.pred
  if (is.null(res)) res <- rep(NA, nrow(newdata))
  res
}



#' Kriging interpolation wrapper
#'
#' @param formula,data,locations See [gstat::krige()].
#'
#' @return `mykrige` returns a list with the formula and data, i.e. an object of class `mykrige`. The predict method return only the predicted values as a numeric vector.
#' @export
mykrige <- function(formula, data, locations = ~x+y, range = 500, nsratio = 0.25, fixed = FALSE) {
  vm <- vgm(psill = 1 - nsratio, model = "Sph",
            range = range, nugget = nsratio)
  if (!fixed) {
    v <- variogram(formula, locations = locations, data = data)
    vm <- fit.variogram(v, vm)
  }
  m <- list(formula = formula, data = data, locations = locations,
            semivariogram = vm)
  class(m) <- "mykrige"
  m
}


#' @describeIn myidw Predict method for `mykrige` objects.
#' @export
predict.mykrige <- function(object, newdata,
                          nmax = 7, ...) {
  if (nrow(newdata) == 0) return(numeric())
  res <- gstat::krige(formula = object$formula,
                    locations = object$locations,
                    object$data,
                    model = object$semivariogram,
                    newdata = newdata, debug.level = 0,
                    nmax = nmax, ...)
  res <- res$var1.pred
  if (is.null(res)) res <- rep(NA, nrow(newdata))
  res
}

