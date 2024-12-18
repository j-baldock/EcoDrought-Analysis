library(quarto)
library(knitr)

# To add a chapter to the book, or shuffle the order of chapters, first render the single chapter, then render the entire book (after updating the .yml file)

# Render all
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

quarto::quarto_render("Big G Little g/GgStoryPlots.qmd", 
                      #cache_refresh = TRUE, # default is FALSE
                      output_format = "html", cache = FALSE)

quarto::quarto_render("Event Delineation/HydroEvents.qmd", 
                      cache_refresh = TRUE, # default is FALSE
                      output_format = "html")

quarto::quarto_render("Big G Little g/WedgeModel.qmd", 
                      cache_refresh = TRUE, # default is FALSE
                      output_format = "html")


# Deprecated

quarto::quarto_render("Big G Little g/GgDiffKS.qmd", 
                      #cache_refresh = TRUE, # default is FALSE
                      output_format = "html")

quarto::quarto_render("Big G Little g/GgDiffJAGS.qmd", 
                      #cache_refresh = TRUE, # default is FALSE
                      output_format = "html")


