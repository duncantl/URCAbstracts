if(FALSE) {

    # Takes 9-10 seconds
    f = "2024-abstract-book.xml"
    z = combinePages(getAbstracts2(f))

    # For 2019 PDF/XML, need to read as latin1 rather than UTF8.
    # Also, calling abstractPages() causes a memory limit error for the XPath.
    # So specify these manually.

    xml = list.files(pattern = "\\.xml$")
    docs = lapply(xml, \(f) tryCatch(readPDFXML(f), error = function(...) readPDFXML(f, encoding = "latin1")))
    system.time({ v = lapply(docs, \(f) try(combinePages(getAbstracts2(f))))})
    names(v) = xml
    v[[1]] = combinePages(getAbstracts2(docs[[1]], pages = doc1[52:233]))

    abs = do.call(rbind, v)
    rownames(abs) = NULL
    abs$year = rep((2019:2025)[-4], sapply(v, nrow))

    mp = c("Microbiology & Molec Gene" = "Microbiology & Molec Genetics",
      "Molecular & Cellular Bio" = "Molecular & Cellular Biology",
      "Land, Air and Water Resources" = "Land Air & Water Resources")
    for(i in names(mp))
        abs$dept[abs$dept == i] = mp[i]

    # Check changed.
    table(abs$dept %in% names(mp))
}


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


    #
    # Microbiology & Molec Gene ->  Microbiology & Molec Genetics
    # Molecular & Cellular Bio -> Molecular & Cellular Biology
    # Land, Air and Water Resources -> "Land Air & Water Resources"
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
    if(is.list(bb) && !is.data.frame(bb))
        bb = do.call(rbind, bb)
    
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

    i = as.integer(getNodeSet(doc, "//page[contains(., 'Sponsor: ')]/@number"))
    i[c(diff(i) == 1, TRUE)]
}

getAbstracts2 =
function(doc, fun = procAbstract, pages = doc[abstractPages(doc)], docFontInfo = getFontInfo(doc), encoding = "UTF8", ...)
{
    if(is.character(doc))
        doc = readPDFXML(doc, encoding = encoding, ...)

    lapply(pages, getPageFontInfo, docFontInfo, fun = fun)
}

combinePages =
function(x)
{
    y = unlist(lapply(x, unlist, FALSE), FALSE)
    ans = do.call(rbind, y)
    tmp = ans$sponsor
    ans$sponsor = cleanSponsorName(tmp)
    ans$dept = cleanDeptName(ans$dept)
    ans
}

rmFooter =
function(bb, lines)    
{
    bb = bb[ bb$top < max(lines$y1) , ]
    i = grep("Annual Undergraduate Research, Scholarship and Creative Activities Conference", bb$text)
    if(length(i))
        bb = bb[ bb$top < min(bb$top[i]) - 5, ]

    bb
}

getPageFontInfo =
function(p, fontInfo, fun = procAbstract, bb = getBBox2(p, TRUE, attrs = c("left", "top", "font")), lines = getBBox(p, TRUE))
{
    bb = rmFooter(bb, lines)

    if(nrow(bb) == 0)
        return(NULL)

    col = split(bb, bb$left >= midPoint(p))
    fi = fontInfo
    bold = fi[fi$isBold | grepl("bold", fi$name, ignore.case = TRUE),]
    pageNum = as.integer(xmlGetAttr(p, "number"))
    lapply(1:length(col), \(i) getColFontInfo(col[[i]], fun, bold, fontInfo, column = i, page = pageNum))
}

    # For each page, 
    # look for title at the top of the two columns
    # Sponsor: to get the font
    # abstract - last several lines of each column
    # department -
    # student name - line above Sponsor:     
getColFontInfo =
function(bb, fun, bold, fontInfo, row = NA, column = NA, page = NA)
{
    bb = bb[order(bb$top),]
    d = diff(bb$top)
    i = which.max(d)
    abs = split(bb, 1:nrow(bb) > i)
    lapply(1:2, function(i) getAbstractFontInfo(abs[[i]], row = i, column = column, page = page, fun, bold, fontInfo))
}

getAbstractFontInfo =
function(bb, fun, bold, fontInfo, row = NA, column = NA, page = NA)    
{
    if(length(bb$text[bb$text != " "]) == 0)
        return(NULL)
    
    bb2 = split(bb, bb$top)
    ll = sapply(bb2, function(x) combineText(x$text[ order(x$left) ]))
    i = grep("Sponsor:", ll)

    fun(title = bb2[ 1:(i-2L) ],
        student = bb2[[ i-1L ]],
        sponsor =  bb2[[ i ]],
        dept = bb2[[ i + 1L ]],
        abstract = bb2[ (i+2L):length(bb2) ],
        row = row,
        column = column,
        page = page
        )
         
}

procAbstract =
function(title, student, sponsor, dept, abstract, row = NA, column = NA, page = NA)
{
    as.data.frame(
        lapply(
            list(title = title,
                 studentName = student,
                 sponsor = sponsor,
                 dept = dept,
                 abstract = abstract,
                 row = row, column = column, pageNum = page),
             
            combineText))
}

abstractFontInfo =
function(title, student, sponsor, dept, abstract)    
{
    list(title = fontByLines(title),
         studentName = student$font,
         abstract = fontByLines(abstract),
         sponsor =  sponsor$font
         )    
}


fontByLines =
function(x)
{
    unique(sapply(x, function(x) x$font))
}

