
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
    if (length(self@filepath) != 1) {
      "@filepath must be a single character value"
    }
  }
)

# virtual functions that will be defined in derived classes ----

# each of these functions needs to be defined in all child classes
read_baseline     <- S7::new_generic("read_baseline", "x")
read_longitudinal <- S7::new_generic("read_longitudinal", "x")

# Generics for all open long data objects ----

# each of these functions will be inherited by all child classes

load <- S7::new_generic("load", "x")

S7::method(load, OpenLongData) <- function(x){
  x@components$baseline <- read_baseline(x)
  x@components$longitudinal <- read_longitudinal(x)
  x
}

as_list <- S7::new_generic("as_list", "x")

S7::method(as_list, OpenLongData) <- function(x){
  list(baseline = x@baseline, longitudinal = x@longitudinal)
}

as_longitudinal <- S7::new_generic("as_longitudinal", "x")

S7::method(as_longitudinal, OpenLongData) <- function(x){
  x@longitudinal
}

as_baseline <- S7::new_generic("as_baseline", "x")

S7::method(as_baseline, OpenLongData) <- function(x){
  x@baseline
}


# Child classes ----

# keeping these in the same file is helpful - it prevents errors that
# may occur when separate files are not sourced in the right order.
OpenLongMesa <- S7::new_class(
  name = "OpenLongMesa",
  package = 'OpenLong',
  parent = OpenLongData,
  validator = function(self) {

    if(length(self@filepath) == 1){

      if("Primary" %nin% list.files(self@filepath)){
        paste0(
          "Primary directory not found in filepath: \'", self@filepath, "\'.",
          "\n- filepath should be the location of",
          " BioLincc MESA data on your device."
        )
      }

    }

  }
)

S7::method(read_baseline, OpenLongMesa) <- function(x){
  # TODO: Simar to add code here for loading baseline files
  # the code below reads in one file given a valid filepath.
  # readr::read_csv(file = file.path(x@filepath,
  #                                  "Primary",
  #                                  "Exam1",
  #                                  "Data",
  #                                  "mesae1ecgcw_drepos_20201102.csv"),
  #                 show_col_types = FALSE)
  tibble::tibble()
}

S7::method(read_longitudinal, OpenLongMesa) <- function(x){
  # TODO: Simar to add code here for loading longitudinal files
  tibble::tibble()
}


OpenLongAbc <- S7::new_class(
  name = "OpenLongAbc",
  package = 'OpenLong',
  parent = OpenLongData,
  validator = function(self) {

    if(length(self@filepath) == 1){

      # TODO: Brian to add a check for valid filepath
      # (ping Byron to discuss this and see example in Mesa object)

    }

  }
)

S7::method(read_baseline, OpenLongAbc) <- function(x){
  # TODO: Brian to add code here for loading baseline files
  tibble::tibble()
}

S7::method(read_longitudinal, OpenLongAbc) <- function(x){
  # TODO: Brian to add code here for loading longitudinal files
  tibble::tibble()
}
