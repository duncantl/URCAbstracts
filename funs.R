library(XML)
library(ReadPDF)


# Problem with 1st abstract on page 145 of 2025.
# Accent on character in sponsor's name means don't get the entire name.

getSponsors = 
function(doc, addPageIdentifier = TRUE)
{
    if(is.character(doc))
        doc = readPDFXML(doc)

    s = getNodeSet(doc, "//text[starts-with(normalize-space(.), 'Sponsor:')]")
    
    s2 = sapply(s, xmlValue)
    ans = data.frame(name = cleanSponsorName(s2), title = getSponsorTitle(s2)) # , txt = s2)
    ans$dept = gsub("&amp;", "&", sapply(s, getDept))
    if(addPageIdentifier)
        ans$page = sapply(s, pageOf)

    ans$dept = cleanDeptName(ans$dept)
    
    ans
}

cleanDeptName =
function(dept)
{
    dept = gsub("&amp;", "&", dept)
    dept = gsub("^((Anr )|Ag (?!&))", "", dept, perl = TRUE)
    dept = gsub("\\(.*\\)$", "", dept)
}


cleanSponsorName =
function(x)    
{
    ans = gsub("Sponsor: ?", "", x)
    gsub(",.*", "", ans) 
}

getSponsorTitle =
function(x)
    trimws(gsub("^[^,]+,", "", x))

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
