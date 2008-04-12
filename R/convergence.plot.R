# ==============================================================================
# convergence plot
# ==============================================================================
convergence.plot <- function( mi.object, ... ) {
  traceplot( mi.object@bugs, ... )
  invisible( mi.object@bugs )
}


conv.plot <- function( mi.object, ... ) {
  invisible( convergence.plot( mi.object = mi.object, ... ) )
}
