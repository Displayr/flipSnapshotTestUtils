#' TestWidget
#'
#' A wrapper to use CompareSnapshot.
#' Compares a widget against a reference snapshot, creating snapshot if
#'  it differs from the reference or the reference does not exist.
#' @param name Used to name the snapshot; does not include file suffix.
#' @param diff.path Directory to place diff snapshot.
#' @param accepted.path Directory to expect accepted snapshots to be.
#'    If no snapshots are found here, new snapshots will be taken of the
#'    widget and these will be considered the new reference.
#' @param ... Other parameters passed to \code{\link{CreateSnapshot}}.
#' @inherit CompareSnapshot
#' @seealso CompareSnapshot
#' @export
TestWidget <- function(widget, 
                       name,
                       delay = 2,
                       threshold = 0.001,
                       ...,
                       diff.path = "snapshots/diff",
                       accepted.path = "snapshots/accepted")
{
    if (!dir.exists(diff.path))
        stop("Directory ", diff.path, " does not exist.")
    if (!dir.exists(accepted.path))
        stop("Directory ", accepted.path, " does not exist.")

    diff.file <- paste0(diff.path, "/", name, ".png")
    accepted.file <- paste0(accepted.path, "/", name, ".png")
    suppressWarnings(CompareSnapshot(widget, diff.file, accepted.file, 
                           delay, threshold, FALSE, ...))
}

    
