
pacman::p_load(shiny, tidyverse, leaflet, viridis, shinythemes, ggthemes, leaflet)


NOMIS_lookup <- readRDS("data/NOMIS_lookup.rds")
UK_geojson <- readRDS("data/UK_geojson.rds")
source("r/functions.r")


regions <- split(NOMIS_lookup, NOMIS_lookup$rgn11nm)




lad_buttons <- function(N) {
  sidebarPanel(
    width = 11,
    radioButtons(
      inputId = paste0("lad_buttons", N),
      label = 'Select Local Authority:',
      choices = sort(regions[[N]]$GEOGRAPHY_NAME),
      inline = T
    )
  )
  
  
}
 

region_tab <- function(N) {
  tabPanel(
    title = names(regions)[N],
    
    
    lad_buttons(N),
    mainPanel(width = 11,
      titlePanel("Map of Local Authority"),
      foodchain_spinner(leafletOutput(paste0("lgd_map", N))),
      titlePanel("Mid Year Estimated Population"),
      foodchain_spinner(DT::dataTableOutput(paste0("pop_region", N))),
      titlePanel("Persons over 85"),
      foodchain_spinner(DT::dataTableOutput(paste0("over85_region", N))),
      titlePanel("Job Seekers Allowance"),
      foodchain_spinner(DT::dataTableOutput(paste0("jsa_region", N))),
      titlePanel("Job Density"),
      foodchain_spinner(DT::dataTableOutput(paste0("jobs_region", N))),
      titlePanel("Number of food production businesses by size"),
      foodchain_spinner(DT::dataTableOutput(paste0("ind_region", N)))
      
    )
    
    
    
  )
  
}






pop_est <- function(geography = NOMIS_lookup$GEOGRAPHY[1]) {
  read_csv(
    paste0(
      "https://www.nomisweb.co.uk/api/v01/dataset/NM_2002_1.data.csv?geography=",
      ifelse(nchar(as.character(geography)) < 1, NOMIS_lookup$GEOGRAPHY[1], geography),
      "&date=latest&c_age=200&measures=20100&select=DATE,GEOGRAPHY_NAME,GENDER_NAME,C_AGE_NAME,OBS_VALUE"
    )
  )%>%
    rename(Date=DATE, `Local Authority`=GEOGRAPHY_NAME,Gender=GENDER_NAME,`Age Category`=C_AGE_NAME,Persons=OBS_VALUE)
  
}

over85<-function(geography){read_csv(paste0("https://www.nomisweb.co.uk/api/v01/dataset/NM_2002_1.data.csv?geography=",geography,"&date=latest&c_age=210&select=DATE,GEOGRAPHY_NAME,GENDER_NAME,C_AGE_NAME,MEASURES_NAME,OBS_VALUE"))%>%
    spread(key = MEASURES_NAME, value = OBS_VALUE)%>%
    rename(Date=DATE, `Local Authority`=GEOGRAPHY_NAME,Gender=GENDER_NAME,`Age Category`=C_AGE_NAME,Persons=Value,`Percent of total population`=Percent)}

jsa <- function(geography) {
  read_csv(
    paste0(
      "https://www.nomisweb.co.uk/api/v01/dataset/NM_4_1.data.csv?geography=",
      geography,
      "&date=latest&sex=7&age_dur=MAKE|Aged%2024%20and%20under|1;2;3;4;5&measures=20100,20301&select=DATE,GEOGRAPHY_NAME,SEX_NAME,AGE_DUR,MEASURES_NAME,OBS_VALUE"
    )
  )
}


industry <- function(geography) {
  read_csv(
    paste0(
      "https://www.nomisweb.co.uk/api/v01/dataset/NM_142_1.data.csv?geography=",
      geography,
      "&date=latest&industry=146800641,146800643,146800650,146800651&employment_sizeband=10,20,30,40&legal_status=0&measures=20100&select=DATE,GEOGRAPHY_NAME,INDUSTRY_NAME,EMPLOYMENT_SIZEBAND_NAME,OBS_VALUE"
    )
  )
  
}

job_density<-function(geography){read_csv(paste0("https://www.nomisweb.co.uk/api/v01/dataset/NM_57_1.data.csv?geography=",geography,"&date=latest&item=1,3&measures=20100&select=DATE,GEOGRAPHY_NAME,ITEM_NAME,OBS_VALUE"))%>%
    spread(key = ITEM_NAME, value = OBS_VALUE)%>%
    rename(Date=DATE, `Local Authority`=GEOGRAPHY_NAME)
}


ui <- fluidPage(
  
  dashboardPage("NOMIS Explorer",
  thead = tagList(
  tags$head(
    
    includeCSS("CSS/mbie-styles.css"),
    includeCSS("CSS/tdstyles.css")
  ),
  div(class = "container-fluid"
      , mbie_header()
  )),
  
  

                           
                           
                           
                           navlistPanel(
                             "Region",
                             
               
                             
                             region_tab(1),
                             region_tab(2),
                             region_tab(3),
                             region_tab(4),
                             region_tab(5),
                             region_tab(6),
                             region_tab(7),
                             region_tab(8),
                             region_tab(9),
                             region_tab(10),
                             region_tab(11),
                             region_tab(12)
                             
                             
                             
                             
                             
                             
              
                          )))


server <- function(input, output, session) {
  lapply(seq_along(regions), function(N) {
    output[[paste0("pop_region", N)]] <-
      DT::renderDataTable(DT::datatable(pop_est(NOMIS_lookup$GEOGRAPHY[NOMIS_lookup$GEOGRAPHY_NAME ==
                                                       input[[paste0("lad_buttons", N)]]]),
                                        
                                        extensions = 'Buttons',
                                        
                                        options = list(
                                          dom = 'Brflit',
                                          paging = FALSE,
                                          searching = TRUE,
                                          fixedColumns = TRUE,
                                          autoWidth = TRUE,
                                          ordering = TRUE,
                                          buttons = c('copy', 'csv', 'excel')
                                        ),
                                        
                                        class = "display")
                          
      )
  })
  
  
  lapply(seq_along(regions), function(N) {
    output[[paste0("over85_region", N)]] <-
      DT::renderDataTable(DT::datatable(over85(NOMIS_lookup$GEOGRAPHY[NOMIS_lookup$GEOGRAPHY_NAME ==
                                                       input[[paste0("lad_buttons", N)]]]),
                                        
                                        extensions = 'Buttons',
                                        
                                        options = list(
                                          dom = 'Brflit',
                                          paging = FALSE,
                                          searching = TRUE,
                                          fixedColumns = TRUE,
                                          autoWidth = TRUE,
                                          ordering = TRUE,
                                          buttons = c('copy', 'csv', 'excel')
                                        ),
                                        
                                        class = "display")
                          
      )
  })
  
  
  lapply(seq_along(regions), function(N) {
    output[[paste0("jsa_region", N)]] <-
      DT::renderDataTable(DT::datatable(jsa(NOMIS_lookup$GEOGRAPHY[NOMIS_lookup$GEOGRAPHY_NAME ==
                                                   input[[paste0("lad_buttons", N)]]]),
                                        
                                        extensions = 'Buttons',
                                        
                                        options = list(
                                          dom = 'Brflit',
                                          paging = FALSE,
                                          searching = TRUE,
                                          fixedColumns = TRUE,
                                          autoWidth = TRUE,
                                          ordering = TRUE,
                                          buttons = c('copy', 'csv', 'excel')
                                        ),
                                        
                                        class = "display")
                          
      )
  })
  
  
  lapply(seq_along(regions), function(N) {
    output[[paste0("ind_region", N)]] <-
      DT::renderDataTable(DT::datatable(industry(NOMIS_lookup$GEOGRAPHY[NOMIS_lookup$GEOGRAPHY_NAME ==
                                                        input[[paste0("lad_buttons", N)]]]),
                                        
                                        extensions = 'Buttons',
                                        
                                        options = list(
                                          dom = 'Brflit',
                                          paging = FALSE,
                                          searching = TRUE,
                                          fixedColumns = TRUE,
                                          autoWidth = TRUE,
                                          ordering = TRUE,
                                          buttons = c('copy', 'csv', 'excel')
                                        ),
                                        
                                        class = "display")
                          
      )
  })
  
  lapply(seq_along(regions), function(N) {
    output[[paste0("jobs_region", N)]] <-
      DT::renderDataTable(DT::datatable(job_density(NOMIS_lookup$GEOGRAPHY[NOMIS_lookup$GEOGRAPHY_NAME ==
                                                        input[[paste0("lad_buttons", N)]]]),
                                        
                                        extensions = 'Buttons',
                                        
                                        options = list(
                                          dom = 'Brflit',
                                          paging = FALSE,
                                          searching = TRUE,
                                          fixedColumns = TRUE,
                                          autoWidth = TRUE,
                                          ordering = TRUE,
                                          buttons = c('copy', 'csv', 'excel')
                                        ),
                                        
                                        class = "display")
                          
      )
  })
  
  
  lapply(seq_along(regions), function(N) {
    output[[paste0("lgd_map", N)]] <-
      renderLeaflet(
        subset(UK_geojson, tolower(UK_geojson$lad19nm) == tolower(input[[paste0("lad_buttons", N)]])) %>%
          leaflet() %>%
          addTiles() %>%
          addPolygons(
            stroke = T,
            smoothFactor = 0.3,
            fillOpacity = 0.3,
            color = viridis::viridis(1)
          )
      )
  })

  


    
    
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
