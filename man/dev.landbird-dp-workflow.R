
devtools::load_all()
devtools::document()
?rm_metadata

# Manage dev packages
.libPaths()

# Install required packages
renv::snapshot()

# Create project folders in directory
usethis::use_directory("doc")

# Add packages to DESCRIPTION
usethis::use_package("devtools")
"C:/Users/hvincelette/AppData/Local/R/cache/R/renv/library/landbird-dp-workflow-001b666c/R-4.2/x86_64-w64-mingw32/mdJSONdictio"
drat::insertPackage()
# Remotes:
#   git::https://github.com/hdvincelette/mdJSONdictio.git

# Create R scripts
usethis::use_r("add_to_metadata")

# Editview metadata
add_metadata(constant = "EDUCATION",
             column_name = "Education",
             display_name = "Education",
             col_type = "str")

rm_metadata("EDUCATION")

extract_metadata("constant")

write_internal_data()

# Run targets workflow
targets::tar_make()
targets::tar_visnetwork()
targets::tar_read(education_vs_bmi)
