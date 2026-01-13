mkJSON =
function(abs, by, file)
{        
    byDept = split(abs, abs[[by]])
    byDept = lapply(byDept, function(x) x[order(x$year, decreasing = TRUE),])
    json = jsonlite::toJSON(byDept, "rows")
    cat("var byDept = \n", json, file = file)
}

mkJSON(abs, "dept", "abstractsByDepartment.json")
mkJSON(abs, "sponsor", "abstractsBySponsor.json")


