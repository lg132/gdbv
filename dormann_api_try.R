library(seaaroundus)

bla <- catchdata(region="fishing-entity", id="66", measure="tonnage", dimension="taxon")
str(bla)


path <- paste("api/v1", region, measure, dimension, "", sep = "/")
path
args <- list(region_id = id, limit = 10)
args

conn <- crul::HttpClient$new(url = seaaroundus:::getapibaseurl(), headers = list(`X-Request-Source` = "r"), 
                             opts = list(followredirects = TRUE))
str(conn)
resp <- conn$get(path = path, query = args)
resp$raise_for_status()
jsonlite::fromJSON(resp$parse("UTF-8"))$data
