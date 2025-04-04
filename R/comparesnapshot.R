#' CompareSnapshot
#'
#' Takes snapshot of a widget and compares it against another snapshot.
#' @return Return \code{FALSE} only if the snapshot of the widget differs from
#'  the accepted snapshot (i.e. returns \code{TRUE} when the accepted snapshot
#'  does not exist). The \code{diff.file} is only created if \code{FALSE} is 
#'  returned otherwise the snapshot will be saved as \code{accepted.file}.
#' @param widget The htmlwidget to display.
#' @param diff.file Character; the name of the snapshot file taken. Only saved if there is a difference.
#' @param accepted.file Character; the name of the snapshot to be compared against.
#'    If this file does not exist, it will be created from a snapshot of the widget.
#' @param delay Time to wait before taking screenshot, in seconds. Sometimes a longer delay is needed for all assets to display properly.
#' @param threshold Similarity parameter for comparing screenshots.
#' @param strict Whether to give warning if \code{accepted.file} does not exist.
#' @param ... Other parameters passed to \code{\link{CreateSnapshot}}.
#' @importFrom visualTest isSimilar
#' @export
CompareSnapshot <- function(widget, 
                            diff.file,
                            accepted.file,
                            delay = 0.2,
                            threshold = 0.1,
                            strict = FALSE,
                            ...)
{
    if (!file.exists(accepted.file))
    {
        CreateSnapshot(widget, filename = accepted.file, delay = delay, ...)
        if (strict)
            warning("File ", accepted.file, " does not exist.")
        return (TRUE)
    }
    else
    {
        CreateSnapshot(widget, filename = diff.file, delay = delay, ...)
        diff.detected <- !isSimilar(file = diff.file, fingerprint = accepted.file, threshold = threshold)
        if (diff.detected && identical(Sys.getenv("CIRCLECI"), "true"))
            file.copy(diff.file, accepted.file, overwrite = TRUE)
        if (!diff.detected || identical(Sys.getenv("CIRCLECI"), "true"))
            unlink(diff.file)

        return(!diff.detected)
    }
}
