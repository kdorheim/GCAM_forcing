
library(rgcam)

queryFile <- "auxiliary_data/hector-queries.xml"
# Create the connection to the database of interest.


# # The release data I've been working with 
# conn      <- localDBConn(dbPath = "GCAM-V7.0-materials/output", dbFile = "database_basexdbGCAM")
# proj_path <- 'gcam_db_ref.dat'

# Actual data from CT's experiments
conn      <- localDBConn("~/Desktop/", dbFile = "DB_food_95_CanESM5_crop")
proj_path <- 'ct_gcam_db.dat'

gcam_data <- addScenario(conn = conn,
                         proj = proj_path,
                         queryFile = queryFile)

# Extract results from all the scenarios in the xml db.
listScenariosInDB(conn)$name %>%
  lapply(function(name){
    gcam_data <- addScenario(conn = conn,
                             proj = proj_path,
                             scenario = name,
                             queryFile = queryFile)
    return(invisible())
  })





