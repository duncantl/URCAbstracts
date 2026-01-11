FontInfo = list(title = 36, studentName = c(62, 76), abstract = c(65, 74, 78), sponsor = c(64, 73))

doPage =
function(p, bb = getBBox2(p, TRUE, attrs = c("left", "top", "font")),
         fonts = FontInfo,
         lines = getBBox(p, TRUE))
{
    if(nrow(lines))
        bb = bb[ bb$top + bb$height < min(lines$y0),]
    v = getProjects(bb, midPoint(p), fonts)
    v$pageNum = as.integer(xmlGetAttr(p, "number"))
    v
}

midPoint =
function(p)
{
    as.integer(xmlGetAttr(p, "width"))/2
}


getProjects =
function(bb, mid, fonts = FontInfo)
{
    cols = split(bb, bb$left > mid) # max(bb$left + bb$width)/2)
    structure(lapply(cols, splitCol, fonts), names = paste0("column", 1:2))
    v0 = lapply(cols, splitCol, fonts)
    v = do.call(rbind, v0)
    v$column = rep(1:2, sapply(v0, nrow))
    v
}

splitCol =
function(bb, fonts = FontInfo)
{
    bb = bb[order(bb$top),]
    d = diff(bb$top)
#    browser()
#    stopifnot(all(d >= 0))
    i = which.max(d) 
    as = split(bb, 1:nrow(bb) > i)
#    w = bb$font %in% fonts$title
#    r = rle(w)
#    bb[w,]

    v = lapply(as, mkAbstract, fonts)
    v = do.call(rbind, v)
    v$row = 1:nrow(v)
    v
}

mkAbstract =
function(bb, fonts = FontInfo)
{
    if(length(bb$text[bb$text != " "]) == 0)
        return(NULL)
    
    w = bb$font %in% fonts$studentName
#    browser()
    ti = bb[bb$top < min(bb$top[w]), ]
    txt = bb[bb$top >= min(bb$top[ bb$font %in% fonts$abstract]), ]

    as.data.frame(lapply(list(title = ti, text = txt, student = bb[w,]),
           #             sponsor = sponsor, department =
                         combineText))
#   list(title = combineText(ti),
#        text = combineText(txt),
#        student = combineText(bb[w,]),
#        sponsor = combineText(sponsor)
#       )
}

combineText =
function(bb)
{
    normalizeSpace( paste(bb$text, collapse = " ") )
}


normalizeSpace =
function(x)    
    trimws(gsub("[[:space:]]+", " ", x))
