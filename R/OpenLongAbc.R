
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
