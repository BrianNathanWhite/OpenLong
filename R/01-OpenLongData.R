
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
    derived      = S7::new_property(S7::class_logical, default = FALSE),
    names_harmonized = S7::new_property(S7::class_logical, default = FALSE)
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

harmonize_names_baseline <- S7::new_generic("harmonize_names_baseline", "x")
harmonize_names_longitudinal <- S7::new_generic("harmonize_names_longitudinal", "x")



# Generics for all open long data objects ----

#' Load files for an OpenLong data set
#'
#' retrieve relevant files for the study of interest
#'
#' @param x an object inheriting from the `OpenLong::OpenLongData` class.
#'
#' @export

data_load <- S7::new_generic("data_load", "x")

S7::method(data_load, OpenLongData) <- function(x){
  S7::prop(x, "components")$baseline <- read_baseline(x)
  S7::prop(x, "components")$longitudinal <- read_longitudinal(x)
  S7::prop(x, "loaded") <- TRUE
  x
}

#' Create new variables within an OpenLong data set
#'
#' generates new variables using data from the study of interest
#'
#' @inheritParams data_load
#'
#' @export
data_derive <- S7::new_generic("data_derive", "x")

S7::method(data_derive, OpenLongData) <- function(x){
  S7::prop(x, "baseline") <- derive_baseline(x)
  S7::prop(x, "longitudinal") <- derive_longitudinal(x)
  S7::prop(x, "derived") <- TRUE
  x
}

#' Clean existing variables within an OpenLong data set
#'
#' Modifies existing variables within data from the study of interest
#'
#' @inheritParams data_load
#'
#' @export
data_clean <- S7::new_generic("data_clean", "x")

S7::method(data_clean, OpenLongData) <- function(x){
  S7::prop(x, "baseline") <- clean_baseline(x)
  S7::prop(x, "longitudinal") <- clean_longitudinal(x)
  S7::prop(x, "cleaned") <- TRUE
  x
}


#' Harmonize variable names of an OpenLong data set
#'
#' Modifies existing variables within data from the study of interest
#'
#' @inheritParams data_load
#'
#' @export
data_harmonize_names <- S7::new_generic("data_harmonize_names", "x")

S7::method(data_harmonize_names, OpenLongData) <- function(x){
  S7::prop(x, "baseline") <- harmonize_names_baseline(x)
  S7::prop(x, "longitudinal") <- harmonize_names_longitudinal(x)
  S7::prop(x, "names_harmonized") <- TRUE
  x
}

S7::method(harmonize_names_baseline, OpenLongData) <- function(x){

  study <- class(x)[1] %>%
    stringr::str_remove("^OpenLong\\:\\:OpenLong") %>%
    stringr::str_to_lower()

  name_x <- paste("name", study, sep = '_') %>%
    purrr::set_names('name_current')

  recoder <- names_guide_baseline %>%
    dplyr::select(name_harmonized, !!name_x) %>%
    tidyr::drop_na() %>%
    dplyr::filter(name_current %in% names(S7::prop(x, 'baseline'))) %>%
    tibble::deframe()

  dplyr::select(S7::prop(x, "baseline"), !!!recoder)

}

S7::method(harmonize_names_longitudinal, OpenLongData) <- function(x){

  S7::prop(x, 'longitudinal')

}

#' Retrieve components from an OpenLong data set
#'
#' OpenLong objects contain longitudinal and baseline components.
#'
#' @inheritParams data_load
#'
#' @export
get_components <- S7::new_generic("get_components", "x")

S7::method(get_components, OpenLongData) <- function(x){
  S7::prop(x, "components")
}

#' Retrieve components from an OpenLong data set
#'
#' OpenLong objects contain longitudinal and baseline components.
#'
#' @inheritParams data_load
#'
#' @export
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


# identifies whether we should be using the base file in components
# or a derived data file that was created previously, e.g., data_derive
# creates an object in the basline slot and if we call data_clean
# after data_derive we would want data_clean to use the data that data_derive
# left. However, if we call data_derive before data_clean we want data_derive
# to use the original components

identify_usable_data <- S7::new_generic("identify_usable_data", "x")

S7::method(identify_usable_data, OpenLongData) <- function(x, data_type){

  data_to_use <- S7::prop(x, data_type)

  if(is_empty(data_to_use)){

    data_to_use <- S7::prop(x, "components")[[data_type]] %>%
      purrr::reduce(dplyr::left_join)

  }

  data_to_use

}



