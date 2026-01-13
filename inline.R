inline =
function(doc, out = character())
{
    if(is.character(doc))
        doc = htmlParse(doc, encoding = "UTF8")

    sc = getNodeSet(doc, "//script[@src]")
    ff = sapply(sc, xmlGetAttr, "src")
    content = sapply(ff, function(x) paste(readLines(x, warn = FALSE), collapse = "\n"))
    
    mapply(inlineScript, sc, content)

    if(length(out) && !is.na(out))
        saveXML(doc, out)
    else
        doc
}

inlineScript =
function(node, content)    
{
    newXMLTextNode(content, parent = node, cdata = TRUE)
    removeAttributes(node, "src")
    node
}
