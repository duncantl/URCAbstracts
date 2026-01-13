# For 2019 PDF/XML, need to read as latin1 rather than UTF8.
# Also, calling abstractPages() causes a memory limit error for the XPath.
# So specify these manually.

xml = list.files(pattern = "\\.xml$")
enc = c("latin1", rep("UTF8", length(xml) - 1L))
docs = mapply(\(f, enc) readPDFXML(f, encoding = enc),
              xml, enc)
system.time({ v = lapply(docs, \(f) try(combinePages(getAbstracts2(f))))})
names(v) = xml
err = sapply(v, inherits, 'try-error')
# stopifnot(!any(err))
v[[1]] = combinePages(getAbstracts2(docs[[1]], pages = docs[[1]][52:233]))

abs = do.call(rbind, v)
rownames(abs) = NULL
abs$year = rep((2019:2025)[-4], sapply(v, nrow))

mp = c("Microbiology & Molec Gene" = "Microbiology & Molec Genetics",
       "Molecular & Cellular Bio" = "Molecular & Cellular Biology",
       "Land, Air and Water Resources" = "Land Air & Water Resources",
       "Neuro Physio & Behavior"  = "Neurobiology, Physiology & Behavior",
       "Neurobiology, Phys & Behav" = "Neurobiology, Physiology & Behavior",
       "Rights" = "Human Rights")

for(i in names(mp))
    abs$dept[abs$dept == i] = mp[i]

# Check changed.
table(abs$dept %in% names(mp))

#saveRDS(abs, "abstracts.rds")

