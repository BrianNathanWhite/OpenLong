# This is S7’s way of registering methods, rather than using export directives
# in your NAMESPACE like S3 and S4 do. This is only strictly necessary if
# registering methods for generics in other packages, but there’s no harm in
# adding it and it ensures that you won’t forget later. (And if you’re not
# importing S7 into your namespace it will quiet an R CMD check NOTE.)

.onLoad <- function(...) {
  S7::methods_register()
}
