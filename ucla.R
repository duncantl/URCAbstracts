if(FALSE) {
    u = readPDFXML("UCLA/McNair2024_Program_Final.xml", encoding = 'UTF8')
    v = getNodeSet(u, "//text[translate(., 'abcdefghijklmnopqrstuvwxyz', 'ABCDEFGHIJKLMNOPQRSTUVWXYZ') = . and @font != 109]")
    v = v[sapply(v, pageOf) > 32]
#    nm = sapply(v, xmlValue)
    tmp = data.frame(name = sapply(v, xmlValue), page = sapply(v, pageOf))
}


ucla = 
function(doc)
{
    if(is.character(doc))
        doc = readPDFXML(doc)

    sessions = sessions(doc)
    abs = getAbstractsUCLA(doc, min(sessions$page):max(sessions$page))
    # connect to session by page number
    c = cut(abs$page, c(sessions$page, Inf), right = FALSE)
    abs$session = sessions$session[c]
    abs
}

sessions =
function(doc, font = "109")    
{
    sess = getNodeSet(u, sprintf("//text[@font = %s]", font))
    sessionTitles = tapply(sess, pg, function(x) combineText(sapply(x, xmlValue)))
    stitles = gsub("Breakout.*", "", sessionTitles)
    stitles = gsub("^July 2[56], 2024 - [0-9:]+ (AM|PM) ", "", stitles)
    data.frame(session = trimws(unname(stitles)), page = as.integer(names(stitles)))
}

getAbstractsUCLA =
function(doc, pageNums)    
{
    do.call(rbind, lapply(doc[pageNums], getPageAbstracts))

#    ti = getNodeSet(u, "//text[@font = 111]")    
}

getPageAbstracts =
function(p, bb = getBBox2(p, TRUE, attrs = c("top", "left", "font")))
{
    bb = bb[bb$font != 109,]
    bb = bb[ bb$text != "Abstract titles link to event detail pages.", ]
    # Won't handle accents. Have to add them explicitly.
    g = split(bb, cumsum(grepl("^[A-Z][-A-ZÁÉÍÓÚÜÑ,.' ]+$", bb$text)))
    stopifnot(length(g) == length(grep("^Location: ", bb$text)))

    do.call(rbind, lapply(g, mkAbstractUCLA, pageOf(p)))
    
    # name and school are on the previous two lines.
    # Handle titles on multiple lines.
    # Get Location: and then down to the next abstract.
}

mkAbstractUCLA =
function(bb, pageNum)
{
    w = bb$font == 111

    data.frame(studentName = bb$text[1],
               university = bb$text[2],
               title = combineText(bb$text[w]),
               abstract = paste(bb$text[-(1:(max(which(w) + 1L)))], collapse = " "),
               page = pageNum
               )
}


if(FALSE) {
    bb = bb[order(bb$top),]
    w = bb$font == 111
    browser()
    g = split(bb, cumsum(bb$font == 111))
    
}
