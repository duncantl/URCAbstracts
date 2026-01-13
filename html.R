byDept = split(abs, abs$dept)
byDept = lapply(byDept, function(x) x[order(x$year, decreasing = TRUE),])
#json = jsonlite::toJSON(byDept, "columns")
json = jsonlite::toJSON(byDept, "rows")
cat("var byDept = \n", json, file = "abstractsByDepartment.json")

