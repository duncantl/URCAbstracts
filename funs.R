library(XML)
library(ReadPDF)
       
getSponsors = 
function(doc, addPageIdentifier = TRUE)
{
    if(is.character(doc))
        doc = readPDFXML(doc)

    s = getNodeSet(doc, "//text[starts-with(normalize-space(.), 'Sponsor:')]")
    
    s2 = sapply(s, xmlValue)
    ans = gsub("Sponsor: ?", "", s2)
    ans = data.frame(name = gsub(",.*", "", ans), title = gsub("^[^,]+,", "", s2)) # , txt = s2)
    ans$dept = gsub("&amp;", "&", sapply(s, getDept))
    if(addPageIdentifier)
        ans$page = sapply(s, pageOf)

    ans
}


getDept =
function(x)
{
    sib = getSibling(x)
    ans = xmlValue(sib, trim = TRUE)
    if(ans == "")
        getDept(sib)
    else
        ans
        
}
