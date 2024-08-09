
OpenLongData <- S7::new_class(
  name = "OpenLongData",
  package = "OpenLong",
  properties = list(
    filepath     = S7::class_character,
    baseline     = S7::class_data.frame,
    longitudinal = S7::class_data.frame,
    components   = S7::class_list,
    loaded       = S7::new_property(S7::class_logical, default = FALSE),
    excluded     = S7::new_property(S7::class_logical, default = FALSE),
    cleaned      = S7::new_property(S7::class_logical, default = FALSE),
    derived      = S7::new_property(S7::class_logical, default = FALSE)
  ),
  validator = function(self) {

    if (length(S7::prop(self, "filepath")) != 1) {
      "filepath must be a single character value"
    }
  }
)

#' Print an open long data object
#'
#' @param x an object of class `OpenLong::OpenLongData`
#' @param ... not currently used
#'
#' @return `x`, invisibly
#'
#' @export
#'
`print.OpenLong::OpenLongData` <- function(x, ...){

  cwidth <- cli::console_width()

  top_label <- "OpenLong dataset "

  ndash <- (cwidth - nchar(top_label))

  dashes <- paste(rep("-", ndash), collapse = '')

  status <- c("loaded", "excluded", "cleaned", "derived")

  for(i in seq_along(status)){
    names(status)[i] <- ifelse(S7::prop(x, status[i]),
                               yes = "v",
                               no = "x")

    status[i] <- stringr::str_to_title(status[i])

  }

  cat(top_label, dashes, "\n")

  cli::cli_bullets(status)

  invisible(x)

}

# virtual functions that will be defined in derived classes ----

# each of these functions needs to be defined in all child classes
read_baseline     <- S7::new_generic("read_baseline", "x")
read_longitudinal <- S7::new_generic("read_longitudinal", "x")

derive_baseline     <- S7::new_generic("derive_baseline", "x")
derive_longitudinal <- S7::new_generic("derive_longitudinal", "x")

clean_baseline     <- S7::new_generic("clean_baseline", "x")
clean_longitudinal <- S7::new_generic("clean_longitudinal", "x")




# Generics for all open long data objects ----



data_load <- S7::new_generic("data_load", "x")

S7::method(data_load, OpenLongData) <- function(x){
  S7::prop(x, "components")$baseline <- read_baseline(x)
  S7::prop(x, "components")$longitudinal <- read_longitudinal(x)
  S7::prop(x, "loaded") <- TRUE
  x
}

data_derive <- S7::new_generic("data_derive", "x")

S7::method(data_derive, OpenLongData) <- function(x){
  S7::prop(x, "baseline") <- derive_baseline(x)
  S7::prop(x, "longitudinal") <- derive_longitudinal(x)
  S7::prop(x, "derived") <- TRUE
  x
}

data_clean <- S7::new_generic("data_clean", "x")

S7::method(data_clean, OpenLongData) <- function(x){
  S7::prop(x, "baseline") <- clean_baseline(x)
  S7::prop(x, "longitudinal") <- clean_longitudinal(x)
  S7::prop(x, "cleaned") <- TRUE
  x
}

get_components <- S7::new_generic("get_components", "x")

S7::method(get_components, OpenLongData) <- function(x){
  S7::prop(x, "components")
}

as_list <- S7::new_generic("as_list", "x")

S7::method(as_list, OpenLongData) <- function(x){
  list(baseline = S7::prop(x, "baseline"),
       longitudinal = S7::prop(x, "longitudinal"))
}

as_longitudinal <- S7::new_generic("as_longitudinal", "x")

S7::method(as_longitudinal, OpenLongData) <- function(x){
  S7::prop(x, "longitudinal")
}

as_baseline <- S7::new_generic("as_baseline", "x")

S7::method(as_baseline, OpenLongData) <- function(x){
  S7::prop(x, "baseline")
}


