library(quarto)
library(knitr)

# To add a chapter to the book, or shuffle the order of chapters, first render the single chapter, then render the entire book (after updating the .yml file)

# Render all
# Need to delete caches and files for the following qmd files before rendering
#  - CollateData
#  - TemporalResolution
#  - PASTA
quarto::quarto_render(output_format = "html")

# Render single chapter only
quarto::quarto_render("index.qmd", 
                      cache_refresh = TRUE, 
                      output_format = "html")

quarto::quarto_render("Data Availability/CollateData.qmd", 
                      cache_refresh = TRUE, # default is FALSE
                      output_format = "html")

quarto::quarto_render("Explore Data/ExploreData.qmd", 
                      #cache_refresh = TRUE, # default is FALSE
                      output_format = "html")

quarto::quarto_render("Explore Data/TemporalResolution.qmd", 
                      cache_refresh = TRUE, # default is FALSE
                      output_format = "html")

quarto::quarto_render("Big G Little g/GgStoryPlots.qmd", 
                      #cache_refresh = TRUE, # default is FALSE
                      output_format = "html", cache = FALSE)

quarto::quarto_render("Event Delineation/WeeklySummary.qmd", 
                      cache_refresh = TRUE, # default is FALSE
                      output_format = "html")

quarto::quarto_render("Event Delineation/HydroEvents.qmd", 
                      cache_refresh = TRUE, # default is FALSE
                      output_format = "html")

quarto::quarto_render("Big G Little g/WedgeModel.qmd", 
                      cache_refresh = TRUE, # default is FALSE
                      output_format = "html")

quarto::quarto_render("Covariates/LandscapeCovariates.qmd", 
                      cache_refresh = TRUE, # default is FALSE
                      output_format = "html")

quarto::quarto_render("Covariates/PASTA.qmd", 
                      cache_refresh = TRUE, # default is FALSE
                      output_format = "html")

quarto::quarto_render("Qualitative/Overview.qmd", 
                      #cache_refresh = TRUE, # default is FALSE
                      output_format = "html")

quarto::quarto_render("Qualitative/SpaceTimeVar.qmd", 
                      #cache_refresh = TRUE, # default is FALSE
                      output_format = "html")

quarto::quarto_render("Qualitative/Hysteresis.qmd", 
                      #cache_refresh = TRUE, # default is FALSE
                      output_format = "html")

quarto::quarto_render("Qualitative/ModeledQ.qmd", 
                      #cache_refresh = TRUE, # default is FALSE
                      output_format = "html")

quarto::quarto_render("Qualitative/LowFlow.qmd", 
                      cache_refresh = TRUE, # default is FALSE
                      output_format = "html")


quarto::quarto_render("Qualitative/Boxes.qmd", 
                      #cache_refresh = TRUE, # default is FALSE
                      output_format = "html")


# Deprecated

quarto::quarto_render("Big G Little g/GgDiffKS.qmd", 
                      #cache_refresh = TRUE, # default is FALSE
                      output_format = "html")

quarto::quarto_render("Big G Little g/GgDiffJAGS.qmd", 
                      #cache_refresh = TRUE, # default is FALSE
                      output_format = "html")


