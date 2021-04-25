#' IDW interpolation wrapper
#'
#' @param formula,data See [gstat::idw()].
#' @param object,locations,nmax,... See [gstat::idw()]
#'
#' @return `myidw` returns a list with the formula and data, i.e. an object of class `myidw`.
#' @export
myidw <- function(formula, data, nmax = Inf) {
  m <- list(formula = formula, data = data, nmax = nmax)
  class(m) <- "myidw"
  m
}


#' @describeIn myidw IDW without trend variables
#' @export
myidw0 <- function(formula, data, nmax = Inf) {
  m <- list(formula = as.formula(paste(all.vars(formula)[1], "~ 1")), data = data, nmax = nmax)
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
                          nmax = object$nmax, ...) {
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
#' @param svgm Semivariogram model, e.g. `"Sph"`
#' @param range Range parameter of spherical semivariogram
#' @param nsratio Nugget-to-sill ratio of spherical semivariogram
#' @param fixed Determines if the semivariogram parameters should be fixed or not; default: `FALSE`.
#' @param fit.ranges When fitting the semivariogram (i.e. `fixed` is `FALSE`), fit the range parameter (`TRUE`, default), or not.
#' @param nmax Maximum number of neighbours to be used for interpolation.
#'
#' @return `mykrige` returns a list with the formula and data, i.e. an object of class `mykrige`.
#' @export
mykrige <- function(formula, data, locations = ~x+y,
                    svgm = "Sph",
                    range = NA, nsratio = 0.25,
                    fixed = FALSE,
                    fit.ranges = TRUE,
                    nmax = Inf) {
  guess_sill <- var(data[, all.vars(formula)[1]]) * 0.5
  vm <- vgm(psill = guess_sill * (1 - nsratio),
            model = svgm,
            range = range,
            nugget = guess_sill * nsratio)
  if (!fixed) {
    v <- variogram(formula, locations = locations, data = data)
    vm <- fit.variogram(v, vm, fit.ranges = fit.ranges)
  }
  m <- list(formula = formula, data = data, locations = locations,
            semivariogram = vm, nmax = nmax)
  class(m) <- "mykrige"
  m
}


#' Predict method for `mykrige` objects
#'
#' @param object,newdata,... See [gstat::krige()].
#'
#' @return The predict method return only the predicted values as a numeric vector.
#' @export
predict.mykrige <- function(object, newdata,
                            nmax = object$nmax, ...) {
  if (nrow(newdata) == 0) return(numeric())
  if (is.null(nmax)) nmax <- Inf
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
  y[sel] <- psill * (1.5*x[sel]/range - 0.5*(x[sel]/range)^3) + nugget
  y
}

