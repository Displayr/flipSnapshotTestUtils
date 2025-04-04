#' CreateSnapshot
#' 
#' Converts a htmlwidget to png image file
#' @param widget The htmlwidget to display.
#' @param filename Name of output file (png format).
#' @param width Width of the snapshot viewport in pixels.
#' @param height Height of the snapshot viewport in pixels.
#' @param delay Time to wait before taking screenshot, in seconds. Sometimes a longer delay is needed for all assets to display properly.
#' @param mouse.hover Logical indicating whether or not to simulate mouse moving.
#'    Note that the cursor itself is not visible, only the effects of clicking.
#' @param mouse.click Logical indicating whether to simulate mouse click.
    #'  Overrides \code{mouse.click}.
#' @param mouse.doubleclick Logical indicating whether to simulate doubleclicking.
#'  Overrides \code{mouse.hover} and \code{mouse.click}.
#' @param mouse.xpos Horizontal position of the mouse ranging from 0 (left) to 1 (right).
#' @param mouse.ypos Vertical position of the mouse ranging from 0 (top) to 1 (bottom).
#' @param debug Print debug message from snapshotting.
#' @details Works with plotly and rhtmlLabeledScatter. Errors with rhtmlPictograph.
#' @importFrom htmlwidgets saveWidget
#' @import chromote
#' @export
CreateSnapshot <- function(widget, filename, delay = 0.2, width = 992, height = 744,
                           mouse.hover = TRUE, mouse.click = FALSE, mouse.doubleclick = FALSE,
                           mouse.xpos = 0.5, mouse.ypos = 0.5, debug = FALSE)
{
    if (inherits(widget, "StandardChart"))
        widget <- widget$htmlwidget
    # Ensure that filename has 'png' suffix which is used by webshot
    if (!grepl(".png$", tolower(filename)))
        filename <- paste0(filename, ".png")
   
    # Create temporary html file
    tmp.files <- tempfile()
    tmp.html <- paste0(tmp.files, ".html")
    on.exit(unlink(tmp.html), add = TRUE) 
    on.exit(unlink(tmp.files, recursive = TRUE), add = TRUE)
    saveWidget(widget, file = tmp.html, selfcontained = FALSE)
   
    b <- ChromoteSession$new(width = width, height = height)
    if (debug)
    {
        b$parent$debug_messages(TRUE)
        print(b$Browser$getVersion())
    }    
    b$Page$navigate(paste0("file://", tmp.html))
    b$Page$loadEventFired()
    
    xpos <- mouse.xpos * width
    ypos <- mouse.ypos * height

    if (mouse.click || mouse.doubleclick)
    {
        b$Input$dispatchMouseEvent(type = "mousePressed", x = xpos, y = ypos, 
                                   button = "left", pointerType = "mouse", 
                                   clickCount = if (mouse.doubleclick) 2 else 1)    
        b$Input$dispatchMouseEvent(type = "mouseReleased", x = xpos, y = ypos, 
                                   button = "left", pointerType = "mouse", 
                                   clickCount = if (mouse.doubleclick) 2 else 1)    
    } else if (mouse.hover)
        b$Input$dispatchMouseEvent(type = "mouseMoved", x = xpos, y = ypos)
    
    if (isTRUE(is.finite(delay)) && delay > 0)
        Sys.sleep(delay)
    
    b$screenshot(filename)
    invisible(b$close())
}