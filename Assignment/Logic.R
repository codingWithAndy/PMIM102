source("DBS_Connection.R");


## Collect GP Practices
display_gp_prac <- function(dbs) {
  gp_names <- gp_practices(dbs)
  print(gp_names)
}