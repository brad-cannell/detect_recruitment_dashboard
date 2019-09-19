# For some reason, some of the datetime variables are missing seconds.
# We have to identify those values and then use a different value to the format
# argument than if seconds aren't missing.
make_posixct <- function(x) {
  x <- dplyr::if_else(
    # Detect if seconds are included
    stringr::str_detect(x,'\\d{1,2}:\\d{1,2}:\\d{1,2}'),
    # If they are:
    as.POSIXct(x, format = "%m/%d/%Y %I:%M:%S %p"),
    # If they aren't:
    as.POSIXct(x, format = "%m/%d/%Y %I:%M %p")
  )
}