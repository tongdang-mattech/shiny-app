list.of.packages <- c("shiny", "httr", "rjson", "rlist", "future", "promises", "purrr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(httr)
library(rjson)
library(glue)

library(shiny)
library(networkD3)

library(rlist)
library(future)
plan(multisession)
library(promises)
library(purrr)

fetchData <- function(type, year){
  data <- list()
  base_url <- "https://www150.statcan.gc.ca/t1/wds/sdmx/statcan/v1/rest/data/"
  switch (type,
          "TotalPrimaryAndSecondaryEnergy" = {
            res <- GET(paste0(base_url, glue("DF_25100029/1.1.?startPeriod={year}&endPeriod={year}")),
                       add_headers(Accept='application/vnd.sdmx.data+json;version=1.0.0-wd'))
            data <- fromJSON(rawToChar(res$content))
          },
          "TotalCoal" = {
            res <- GET(paste0(base_url, glue("DF_25100029/1.29.?startPeriod={year}&endPeriod={year}")),
                       add_headers(Accept='application/vnd.sdmx.data+json;version=1.0.0-wd'))
            data <- fromJSON(rawToChar(res$content))
          },
          "CrudeOil" = {
            res <- GET(paste0(base_url, glue("DF_25100029/1.4.?startPeriod={year}&endPeriod={year}")),
                       add_headers(Accept='application/vnd.sdmx.data+json;version=1.0.0-wd'))
            
            data <- fromJSON(rawToChar(res$content))
          },
          "NaturalGas" = {
            res <- GET(paste0(base_url, glue("DF_25100029/1.5.?startPeriod={year}&endPeriod={year}")),
                       add_headers(Accept='application/vnd.sdmx.data+json;version=1.0.0-wd'))
            
            data <- fromJSON(rawToChar(res$content))
          },
          "NGL" = {
            res <- GET(paste0(base_url, glue("DF_25100029/1.6.?startPeriod={year}&endPeriod={year}")),
                       add_headers(Accept='application/vnd.sdmx.data+json;version=1.0.0-wd'))
            
            data <- fromJSON(rawToChar(res$content))
          },
          "Primary" = {
            res <- GET(paste0(base_url, glue("DF_25100029/1.7.?startPeriod={year}&endPeriod={year}")),
                       add_headers(Accept='application/vnd.sdmx.data+json;version=1.0.0-wd'))
            
            data <- fromJSON(rawToChar(res$content))
          },
          "Steam" = {
            res <- GET(paste0(base_url, glue("DF_25100029/1.8.?startPeriod={year}&endPeriod={year}")),
                       add_headers(Accept='application/vnd.sdmx.data+json;version=1.0.0-wd'))
            
            data <- fromJSON(rawToChar(res$content))
          },
          "RenewableFuels" = {
            res <- GET(paste0(base_url, glue("DF_25100029/1.35.?startPeriod={year}&endPeriod={year}")),
                       add_headers(Accept='application/vnd.sdmx.data+json;version=1.0.0-wd'))
            
            data <- fromJSON(rawToChar(res$content))
          },
          "Coke" = {
            res <- GET(paste0(base_url, glue("DF_25100029/1.10.?startPeriod={year}&endPeriod={year}")),
                       add_headers(Accept='application/vnd.sdmx.data+json;version=1.0.0-wd'))
            
            data <- fromJSON(rawToChar(res$content))
          },
          "CokeOvenGas" = {
            res <- GET(paste0(base_url, glue("DF_25100029/1.11.?startPeriod={year}&endPeriod={year}")),
                       add_headers(Accept='application/vnd.sdmx.data+json;version=1.0.0-wd'))
            
            data <- fromJSON(rawToChar(res$content))
          },
          "TotalRefinedProducts" = {
            res <- GET(paste0(base_url, glue("DF_25100029/1.14.?startPeriod={year}&endPeriod={year}")),
                       add_headers(Accept='application/vnd.sdmx.data+json;version=1.0.0-wd'))
            
            data <- fromJSON(rawToChar(res$content))
          },
          "Secondary" = {
            res <- GET(paste0(base_url, glue("DF_25100029/1.13.?startPeriod={year}&endPeriod={year}")),
                       add_headers(Accept='application/vnd.sdmx.data+json;version=1.0.0-wd'))
            
            data <- fromJSON(rawToChar(res$content))
          },
  )
  return(data)
}

getValue <- function(object, name) {
  total <- 0
  values <- object$structure$dimensions$series[[3]]$values
  values <- values[unlist(lapply(values, function(item) grepl(name, item$name, ignore.case=TRUE)))]
  indexs <- c()
  for (value in values) {
    indexs <- append(indexs, which(sapply(object$structure$dimensions$series[[3]]$values, function(x) x$name == value$name))[1])
  }
  indexs <- indexs - 1
  for (idx in indexs) {
    value <- object$dataSets[[1]]$series[paste0('0:0:',idx)][[1]]$observations$`0`[[1]]
    if (is.nan(value) || value == "NaN") {
      total <- total + 0
    } else {
      total <- total + round(as.numeric(value) / 1000)
    }
  }
  return(total)
}

getTotalSupply <- function(object) {
  series <- object$structure$dimensions$series[[3]]$values
  total <- 0
  for (idx in 1:length(series)) {
    if (as.integer(series[[idx]]$id) <= 18) {
      obs <- object$dataSets[[1]]$series[paste0('0:0:',idx-1)][[1]]$observations$`0`[[1]]
      if (!is.na(obs) && obs != 'NaN') {
        total <- total + round(as.integer(obs) / 1000)
      }
    }
  }
  return(total)
}

ui <- fluidPage(
  selectInput(inputId = "year_select",
              label   = "Year",
              choices =  c(1995:2021),
              selected = 2018),
  
  sankeyNetworkOutput("diagram")
)

server <- function(input, output) {
  observeEvent(input$year_select, {
    output$diagram <- NULL
    year <- input$year_select
    total_primary_and_secondary_energry <- future(fetchData("TotalPrimaryAndSecondaryEnergy", year))
    total_coal <- future(fetchData("TotalCoal", year))
    crude_oil <- future(fetchData("CrudeOil", year))
    natural_gas <- future(fetchData("NaturalGas", year))
    ngl <- future(fetchData("NGL", year))
    primary <- future(fetchData("Primary", year))
    steam <- future(fetchData("Steam", year))
    renewable_fuels <- future(fetchData("RenewableFuels", year))
    coke <- future(fetchData("Coke", year))
    coke_oven_gas <- future(fetchData("CokeOvenGas", year))
    total_refined_products <- future(fetchData("TotalRefinedProducts", year))
    secondary <- future(fetchData("Secondary", year))
    
    total_primary_and_secondary_energry <- value(total_primary_and_secondary_energry)
    total_coal <- value(total_coal)
    crude_oil <- value(crude_oil)
    natural_gas <- value(natural_gas)
    ngl <- value(ngl)
    primary <- value(primary)
    steam <- value(steam)
    renewable_fuels <- value(renewable_fuels)
    coke <- value(coke)
    coke_oven_gas <- value(coke_oven_gas)
    total_refined_products <- value(total_refined_products)
    secondary <- value(secondary)
    
    data <- list(
      list(source = "Coal prod", target = "Fossil Fuels", value = getValue(total_coal, "production")),
      list(source = "Natural Gas prod", target = "Fossil Fuels", value = getValue(natural_gas, "production")),
      list(source = "Crude Oil prod", target = "Fossil Fuels", value = getValue(crude_oil, "production")),
      list(source = "NGL's prod", target = "Fossil Fuels", value = getValue(ngl, "production")),
      list(source = "Primary Electricity prod", target = "Domestic Production", value = getValue(primary, "production")),
      list(source = "Secondary Electricity prod", target = "Domestic Production", value = getValue(secondary, "production")),
      list(source = "Steam prod", target = "Domestic Production", value = getValue(steam, "production")),
      list(source = "Crude Oil and Refined Products", target = "Imported Energy", value = getValue(crude_oil, "imports") + getValue(total_refined_products, "imports")),
      list(source = "Other Imports", target = "Imported Energy", value = getValue(total_primary_and_secondary_energry, "imports") - getValue(crude_oil, "imports") - getValue(total_refined_products, "imports")),
      list(source = "Coke, coke oven gas and Adjustments", target = "Total Supply", value = getTotalSupply(coke) + getTotalSupply(coke_oven_gas)),
      list(source = "Fossil Fuels", target = "Domestic Production", value = getValue(total_coal, "production") + getValue(natural_gas, "production") + getValue(crude_oil, "production") + getValue(ngl, "production"))
    )
    
    data <- append(data, list(
      list(source = "Domestic Production", target = "Total Supply", value = data[[11]]$value + data[[5]]$value + data[[6]]$value + data[[7]]$value),
      list(source = "Imported Energy", target = "Total Supply", value = data[[8]]$value + data[[9]]$value),
      list(source = "Total Supply", target = "Crude Oil export", value = getValue(crude_oil, "exports")),
      list(source = "Total Supply", target = "Natural Gas export", value = getValue(natural_gas, "exports")),
      list(source = "Total Supply", target = "Other export", value = getValue(total_primary_and_secondary_energry, "exports") - getValue(crude_oil, "exports") - getValue(natural_gas, "exports")),
      list(source = "Crude Oil export", target = "Exported Energy", value = getValue(crude_oil, "exports")),
      list(source = "Natural Gas export", target = "Exported Energy", value = getValue(natural_gas, "exports")),
      list(source = "Other export", target = "Exported Energy", value = getValue(total_primary_and_secondary_energry, "exports") - getValue(crude_oil, "exports") - getValue(natural_gas, "exports")),
      list(source = "Total Supply", target = "Coal, coke, Coke Oven Gas", value = getValue(total_coal, "availability") + getValue(coke, "availability") + getValue(coke_oven_gas, "availability")),
      list(source = "Total Supply", target = "Natural Gas", value = getValue(natural_gas, "availability")),
      list(source = "Total Supply", target = "NGL's", value = getValue(ngl, "availability")),
      list(source = "Total Supply", target = "Petroleum Products", value = getValue(total_refined_products, "availability")),
      list(source = "Total Supply", target = "Steam", value = getValue(steam, "availability")),
      list(source = "Total Supply", target = "Primary Electricity", value = getValue(primary, "availability")),
      list(source = "Total Supply", target = "Secondary Electricity", value = getValue(secondary, "availability"))
    ))
    
    data <- append(data, list(
      list(source = "Steam", target = "Total Consumption", value = data[[24]]$value),
      list(source = "Primary Electricity", target = "Total Electricity", value = data[[25]]$value),
      list(source = "Secondary Electricity", target = "Total Electricity", value = data[[26]]$value),
      list(source = "Coal, coke, Coke Oven Gas", target = "Fossil Fuel", value = data[[20]]$value),
      list(source = "Natural Gas", target = "Fossil Fuel", value = data[[21]]$value),
      list(source = "NGL's", target = "Fossil Fuel", value = data[[22]]$value),
      list(source = "Petroleum Products", target = "Fossil Fuel", value = data[[23]]$value)
    )
    )
    
    data <- append(data, list(
      list(
        source = "Fossil Fuel",
        target = "Total Consumption",
        value = data[[30]]$value + data[[31]]$value + data[[32]]$value + data[[33]]$value
      ),
      list(source = "Total Electricity", target = "Total Consumption", value = data[[28]]$value + data[[29]]$value),
      list(
        source = "Total Consumption",
        target = "Commercial and Public Admin",
        value = getValue(total_primary_and_secondary_energry, "commercial") + getValue(total_primary_and_secondary_energry, "public administration")
      ),
      list(source = "Total Consumption", target = "Industrial Use", value = getValue(total_primary_and_secondary_energry, "total industrial")),
      list(
        source = "Total Consumption",
        target = "Transportation Use",
        value = getValue(total_primary_and_secondary_energry, "transportation")
      ),
      list(
        source = "Total Consumption",
        target = "Transformation and Adjustments",
        value = getValue(total_primary_and_secondary_energry, "transform") + getValue(total_primary_and_secondary_energry, "adjustments") + getValue(total_primary_and_secondary_energry, "transfers") - getValue(total_primary_and_secondary_energry, "transformed to refined petroleum products")
      ),
      list(source = "Total Consumption", target = "Non-energy Use", value = getValue(total_primary_and_secondary_energry, "non-energy use")),
      list(
        source = "Total Consumption",
        target = "Producer Consumption",
        value = getValue(total_primary_and_secondary_energry, "producer consumption")
      )
    ))
    
    data <- append(data, list(
      list(source = "Total Consumption",
           target = "Residential and Agriculture",
           value = data[[34]]$value + data[[35]]$value - (data[[36]]$value + data[[37]]$value + data[[38]]$value + data[[39]]$value + data[[40]]$value + data[[41]]$value))
    ))
    
    names <- c( "Coal prod", "Natural Gas prod", "Crude Oil prod", "NGL's prod", "Primary Electricity prod",
                "Secondary Electricity prod", "Crude Oil and Refined Products", "Steam prod", "Other Imports", "Coke, coke oven gas and Adjustments",
                "Fossil Fuels", "Imported Energy", "Domestic Production", "Total Supply", "Crude Oil export",
                "Natural Gas export", "Other export", "Primary Electricity", "Secondary Electricity", "Coal, coke, Coke Oven Gas",
                "Natural Gas", "NGL's", "Petroleum Products", "Steam", "Exported Energy",
                "Total Electricity", "Fossil Fuel", "Total Consumption", "Commercial and Public Admin", "Industrial Use",
                "Transportation Use", "Transformation and Adjustments", "Non-energy Use", "Producer Consumption", "Residential and Agriculture")
    
    sources <- sapply(data, function(item) {
      source <- which(names == item$source)
      return(source)
    })
    sources <- sources - 1
    
    targets <- sapply(data, function(item) {
      source <- which(names == item$target)
      return(source)
    })
    targets <- targets - 1
    
    values <- sapply(data, function(item) {
      return(item$value)
    })
    
    lstSpending <- list(
      nodes = data.frame(
        name = names, stringAsFactors = FALSE
      ),
      links = data.frame(
        source = sources,
        target = targets,
        value = values
      )
    )
    
    output$diagram <- renderSankeyNetwork({
      sankeyNetwork(Links = lstSpending$links, Nodes = lstSpending$nodes, Source = "source",
                    Target = "target", Value = "value", NodeID = "name",
                    units = "PJ", fontSize = 12, nodeWidth = 30)
    })
  })
}

shinyApp(ui = ui, server = server)