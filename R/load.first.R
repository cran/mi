.onAttach <- function(...) {
  mylib <- dirname(system.file(package = "mi"))
  ver <- packageDescription("mi", lib.loc = mylib)$Version
  builddate <- packageDescription("mi", lib.loc = mylib)$Date
  msg <- paste("\nmi (Version ", ver, ", built: ", builddate, ")\n", sep = "")
  packageStartupMessage(msg, appendLF = FALSE)
}
