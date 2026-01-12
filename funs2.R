if(FALSE) {

    xml = "25_URC_Abstract_Book_V2-1.xml"
    doc = readPDFXML(xml, encoding = "UTF8")
    pgs = doc[49:313]
    a = lapply(pgs, \(p) try(doPage(p)))
    e = sapply(a, inherits, 'try-error')
    b = do.call(rbind, a)
    rownames(b) = NULL
    b$numWords = sapply(strsplit(b$text, " +"), length)
    b$dept = cleanDeptName(b$dept)
    
    stopifnot(length(grep("Sponsor", b$sponsor)) == 0)
}

if(FALSE) {

    # no longer needed as we are computing sponsor and dept in doPage()
    
    sp = getSponsors(doc)
    sp = sp[sp$page >= 49,]
    # 2 currently not the same due to different fonts for accents.
#    stopifnot(b$sponsor == sp$name)
    b$dept = sp$dept
}

if(FALSE) {

    # Finding department names that are similar.

    d = sort(unique(b$dept))
    dt = adist(d)
    dimnames(dt) = list(d, d)
    tmp = structure(sapply(1:nrow(dt), \(i) { x = dt[i, -i]; names(x)[ x < 5]}), names = d)
    tmp[sapply(tmp, length) > 0]

    # Anr Plant Sciences and Plant Sciences
    # Ag Molecular & Cellular Bio
    # Int Med Cardiology (sac) and Int Med Cardiology (davis)
}


FontInfo = list(title = 36, studentName = c(62, 76), abstract = c(65, 74, 78), sponsor = c(64, 73))

getAbstracts =
function(doc, pages = doc[abstractPages(doc)], fonts = FontInfo, ...)    
{
    if(is.character(doc))
        doc = readPDFXML(doc, ...)

    a = lapply(pages, \(p) try(doPage(p)))
    e = sapply(a, inherits, 'try-error')
    if(any(e))
        warning("errors reading ", sum(e), " abstracts")

    b = do.call(rbind, a)
    rownames(b) = NULL
    b$numWords = sapply(strsplit(b$text, " +"), length)
    b$dept = cleanDeptName(b$dept)
    b
}


doPage =
function(p, bb = getBBox2(p, TRUE, attrs = c("left", "top", "font")),
         fonts = FontInfo,
         lines = getBBox(p, TRUE))
{
    if(nrow(lines)) {
        lines = lines[(lines$x1 - lines$x0) > 30,]
        bb = bb[ (bb$top + bb$height) < min(lines$y0),]
    }
   

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

    v0 = lapply(cols, splitCol, fonts)
    v = do.call(rbind, v0)
    v$column = rep(1:2, sapply(v0, nrow))
    tmp = v$sponsor
    v$sponsor = cleanSponsorName(tmp)
    v$sponsorTitle = getSponsorTitle(tmp)
    v
}

splitCol =
function(bb, fonts = FontInfo)
{
    bb = bb[order(bb$top),]
    d = diff(bb$top)
    #    stopifnot(all(d >= 0))

    i = which.max(d) 
    as = split(bb, 1:nrow(bb) > i)

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

    ans = as.data.frame(lapply(list(title = ti, text = txt, student = bb[w,]),
           #             sponsor = sponsor, department =
                               combineText))
    ans$sponsor = getSponsorText(bb)
    ans$dept = getSponsorDept(bb)
    ans
}


getSponsorText =
function(bb, threshold = 2)    
{
    i = grep("Sponsor:", bb$text)
    bb2 = bb[ abs(bb$top - bb$top[i]) < threshold, ]
    #    ReadPDF:::combineLines(bb2)
    combineText(bb2$text[order(bb2$left)])
}

getSponsorDept =
function(bb, threshold = 2)    
{
    i = grep("Sponsor:", bb$text)
    bb2 = bb[ bb$top > bb$top[i], ]
    bb2 = bb2[ bb2$top == min(bb2$top),]
    combineText(bb2)
}


combineText =
function(bb)
{
    if(is.data.frame(bb))
        bb = bb$text
    normalizeSpace( paste(bb, collapse = "") )
}


normalizeSpace =
function(x)    
    trimws(gsub("[[:space:]]+", " ", x))


abstractPages =
function(doc)
{
    if(is.character(doc))
        doc = readPDFXML(doc, ...)

    as.integer(getNodeSet(doc, "//page[contains(., 'Sponsor: ')]/@number"))
}

getFontInfo =
function(doc, pages = doc[abstractPages(doc)])
{
    if(is.character(doc))
        doc = readPDFXML(doc, ...)

    bbs = lapply(pages, getBBox2, TRUE, attrs = c("left", "top", "font"))
    # For each page, 
    # look for title at the top of the two columns
    # Sponsor: to get the font
    # abstract - last several lines of each column
    # department -
    # student name - line above Sponsor: 
    sapply(
}
