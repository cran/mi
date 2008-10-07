.onAttach <- function(...) {
  mylib <- dirname(system.file(package = "mi"))
  ver <- packageDescription("mi", lib = mylib)$Version
  builddate <- packageDescription("mi", lib = mylib)$Date
  cat(paste("\nmi (Version ", ver, ", built: ", builddate, ")\n", sep = ""))
  if(!any(search()=="package:arm"))
    require(mi)
}
