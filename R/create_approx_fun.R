
#' Title
#'
#' @param f R function taking a single input
#' @param from Minimum x to evaluate
#' @param to Maximum x to evaluate
#' @param N Number of points to evaluate function at
#' @param K Number of knots to use when fitting the spline,
#' or a vector of knot points.
#' @param language Output type of the function. One of "R", "Rchar" for and R
#' function as a string, "JS" for JavaScript,
#' @param varname The name of the variable for the output function
#' @param vectorize_JS If the output is a JavaScript function, should code
#' @param pretty Should the JavaScript output have new lines and tab indentation?
#' be added so it is vectorized?
#' @importFrom utils capture.output
#'
#' @return A function or string
#' @export
#'
#' @examples
#'
#' f1 <- function(x) {cos(exp(2*x^1.4))}
#' curve(f1, lwd=10)
#' f2 <- create_approx_fun(f1,0,1, 11, 4)
#' curve(f2, add=T, col=2, lwd=6, lty=2)
#' f3 <- create_approx_fun(f1,0,1, 51, 14)
#' curve(f3, add=T, col=3, lwd=6, lty=3)
#' # Fewer points but concentrated in right end
#' f4 <- create_approx_fun(f1,0,1, 51, K=c(0,.3,.6,.7,.8,.85,.9,.95,.97,1))
#' curve(f4, add=T, col=4, lwd=6, lty=4)e
#'
#' create_approx_fun(f1,0,1, 11, 4, language='rchar')
#' cat(create_approx_fun(f1,0,1, 11, 4, language='Js'))
#' cat(create_approx_fun(f1,0,1, 11, 4, language='Js', varname = "gamma"))
#' cat(create_approx_fun(f1,0,1, 11, 4, language='Js', vectorize_JS = T))
#' cat(create_approx_fun(f1,0,1, 11, 4, language='Js', vectorize_JS = T, varname="theta"))
#' cat(create_approx_fun(f1,0,1, 11, 4, language='Js'))
#' cat(create_approx_fun(f1,0,1, 11, 4, language='Js', pretty=FALSE))
create_approx_fun <- function(f, from, to, N, K, language="R", varname="x",
                              vectorize_JS=FALSE, pretty=TRUE) {
  # if (missing(knot_x)) {
  x <- seq(from, to, l=N)
  y <- sapply(x, f)
  # } else {
  # K <- knot_x
  # }

  f <- rms::ols(y ~ rcs(x, K))  # 2 d.f. for x

  if (tolower(language) == "r") {
    Hmisc::Function(f)
  } else if (tolower(language) == "rchar") {
    capture.output(Hmisc::Function(f))[[1]]
  } else if (tolower(language) %in% c("js", "javascript")) {
    # browser()
    rchar <- capture.output(Hmisc::Function(f))[[1]]
    rchar <- gsub("pmax(", "Math.pow(Math.max(", rchar, fixed=TRUE)
    rchar <- gsub(")^3", "), 3)", rchar, fixed=TRUE)
    rchar <- gsub(" = NA", "", rchar, fixed=TRUE)
    rchar <- gsub("{", "{\n\treturn ", rchar, fixed=TRUE)
    rchar <- gsub("}", ";\n}", rchar, fixed=TRUE)
    if (varname != "x") {
      rchar <- gsub(" x", varname, rchar, fixed=TRUE)
      rchar <- gsub("(x", paste0("(", varname), rchar, fixed=TRUE)
    }
    if (vectorize_JS) {

      # rchar <- gsub("{return ", "{return (typeof(x)=='number' ? [x] :x).map(x=>(", rchar, fixed=TRUE)
      # rchar <- gsub(";}", "));}", rchar, fixed=TRUE)

      # browser()
      rchar <- gsub("{\n\treturn ",
                    paste0("{let deob = false; if(typeof(", varname,
                           ")=='number') {", varname, "=[", varname,
                           "]; deob=true;} let y=x.map(x=>("),
                    rchar, fixed=TRUE)
      rchar <- paste(rchar, "end")
      rchar <- gsub(";\n} end", "));\n\treturn deob ? y[0] : y;\n}", rchar, fixed=TRUE)
    }
    if (pretty) {
      rchar <- gsub(")-", ") +\n\t-", rchar, fixed=TRUE)
      rchar <- gsub(")+", ") +\n\t",  rchar, fixed=TRUE)
    } else {
      rchar <- gsub("\n", "",  rchar, fixed=TRUE)
      rchar <- gsub("\t", "",  rchar, fixed=TRUE)
    }
    rchar
  }

}
if (F) {
  require(rms)
  f1 <- function(x) {cos(exp(2*x^1.4))}
  curve(f1, lwd=10)
  f2 <- create_approx_fun(f1,0,1, 11, 4)
  curve(f2, add=T, col=2, lwd=6, lty=2)
  f3 <- create_approx_fun(f1,0,1, 51, 14)
  curve(f3, add=T, col=3, lwd=6, lty=3)
  # Fewer points but concentrated in right end
  f4 <- create_approx_fun(f1,0,1, 51, K=c(0,.3,.6,.7,.8,.85,.9,.95,.97,1))
  curve(f4, add=T, col=4, lwd=6, lty=4)

  create_approx_fun(f1,0,1, 11, 4, language='rchar')
  cat(create_approx_fun(f1,0,1, 11, 4, language='Js'))
  cat(create_approx_fun(f1,0,1, 11, 4, language='Js', varname = "Larry"))
  cat(create_approx_fun(f1,0,1, 11, 4, language='Js', vectorize_JS = T))
  cat(create_approx_fun(f1,0,1, 11, 4, language='Js', vectorize_JS = T, varname="theta"))
  cat(create_approx_fun(f1,0,1, 11, 4, language='Js'))
  cat(create_approx_fun(f1,0,1, 11, 4, language='Js', pretty=FALSE))
}
