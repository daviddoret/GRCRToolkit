
# Timezone setting is required for packaging building.
# I pick Geneva arbitrarily.

Sys.getenv("TZ")

if(is.null(Sys.getenv("TZ"))){
  Sys.setenv(TZ="Europe/Geneva")
}

Sys.getenv("TZ")

# as.POSIXct(t, tz=getOption("tz"))
