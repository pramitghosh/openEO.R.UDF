# this is just a temporal fix for an issue during subsetting stars objects with an variable that was defined not in the basenv environment

"[.stars" = function(x, i = TRUE, ..., drop = FALSE, crop = TRUE) {
    missing.i = missing(i)
    # special case:
    if (! missing.i && inherits(i, c("sf", "sfc", "bbox")))
        return(st_crop(x, i, crop = crop))
    
    mc <- match.call(expand.dots = TRUE)
    # select list elements from x, based on i:
    d = attr(x, "dimensions")
    ed = stars:::expand_dimensions.dimensions(d)
    x = unclass(x)[i]
    # selects also on dimensions:
    if (length(mc) > 3) {
        mc[[1]] <- `[`
        if (! missing(i))
            mc[[3]] <- NULL # remove i
        mc[["drop"]] = FALSE
        for (i in names(x)) {
            mc[[2]] = as.name(i)
            x[[i]] = eval(mc, x, enclos = parent.frame())
        }
        mc0 = mc[1:3] # "[", x, first dim
        j = 3 # first dim
        for (i in names(d)) {
            mc0[[2]] = as.name(i)
            mc0[[3]] = mc[[j]]
            mc0[["values"]] = ed[[i]]
            d[[i]] = eval(mc0, d, enclos = parent.frame())
            j = j + 1
        }
    }
    if (drop)
        adrop(st_as_stars(x, dimensions = d))
    else
        st_as_stars(x, dimensions = d)
}