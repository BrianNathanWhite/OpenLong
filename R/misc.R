

# opposite of %in%, helpful for validation functions
`%nin%` = Negate(`%in%`)

# helpful to check is something has been specified or not
is_empty <- function(x) length(x) == 0
