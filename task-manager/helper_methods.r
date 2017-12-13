isNullOrEmpty = function(string) {
  is.null(string) || string == ""
}

getCenteredShinyColumn = function(columnContentString) {
  column(
    12,
    align="center",
    h3(columnContentString)
  )
}