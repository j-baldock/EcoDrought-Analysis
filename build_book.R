library(quarto)
library(knitr)

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

quarto::quarto_render("Big G Little g/GgDiffKS.qmd", 
                      #cache_refresh = TRUE, # default is FALSE
                      output_format = "html")

quarto::quarto_render("Big G Little g/GgDiffJAGS.qmd", 
                      #cache_refresh = TRUE, # default is FALSE
                      output_format = "html")

quarto::quarto_render("Big G Little g/HydroEvents.qmd", 
                      #cache_refresh = TRUE, # default is FALSE
                      output_format = "html")
