#' IDW interpolation wrapper
#'
#' @param formula,data See [gstat::idw()].
#' @param object,locations,nmax,... See [gstat::idw()]
#'
#' @return `myidw` returns a list with the formula and data, i.e. an object of class `myidw`.
#' @export
myidw <- function(formula, data) {
  m <- list(formula = formula, data = data)
  class(m) <- "myidw"
  m
}


#' @describeIn myidw IDW without trend variables
#' @export
myidw0 <- function(formula, data) {
  m <- list(formula = as.formula(paste(all.vars(formula)[1], "~ 1")), data = data)
  class(m) <- "myidw"
  m
}



#' Predict method for `myidw` objects.
#'
#' @param object,newdata,locations,nmax See [gstat::idw()].
#'
#' @return  The predict method returns only the predicted values as a numeric vector.
#' @export
predict.myidw <- function(object, newdata, locations = ~x+y,
                          nmax = 30, ...) {
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
#' @param range Range parameter of spherical semivariogram
#' @param nsratio Nugget-to-sill ratio of spherical semivariogram
#' @param fixed Determines if the semivariogram parameters should be fixed or not; default: `FALSE`.
#'
#' @return `mykrige` returns a list with the formula and data, i.e. an object of class `mykrige`.
#' @export
mykrige <- function(formula, data, locations = ~x+y,
                    range = 500, nsratio = 0.25,
                    fixed = FALSE) {
  guess_sill <- var(data[, all.vars(formula)[1]]) * 0.75
  vm <- vgm(psill = guess_sill * (1 - nsratio),
            model = "Sph",
            range = range,
            nugget = guess_sill * nsratio)
  if (!fixed) {
    v <- variogram(formula, locations = locations, data = data)
    vm <- fit.variogram(v, vm)
  }
  m <- list(formula = formula, data = data, locations = locations,
            semivariogram = vm)
  class(m) <- "mykrige"
  m
}


#' Predict method for `mykrige` objects
#'
#' @param object,newdata,nmax,... See [gstat::krige()].
#'
#' @return The predict method return only the predicted values as a numeric vector.
#' @export
predict.mykrige <- function(object, newdata,
                            nmax = 30, ...) {
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



#' Spherical semivariogram
#'
#' @param x Lag distance (numeric vector)
#' @param range Range
#' @param psill Partial sill (i.e. excluding nugget)
#' @param nugget Nugget semivariance
#'
#' @return Semivariances at the specified distances.
#' @export
svgm_spherical <- function(x, range, psill, nugget = 0) {
  sill <- psill + nugget
  y <- rep(sill, length(x))
  y[ x == 0 ] <- 0
  sel <- (x > 0) & (x < range)
  y[sel] <- sill * (1.5*x[sel]/range - 0.5*(x[sel]/range)^3)
  y
}

