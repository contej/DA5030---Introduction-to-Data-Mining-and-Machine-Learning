rm(list = ls())  # clears environment
cat("\014") # clears the console in RStudio

# Set working directory
setwd("C:/R/DA5030/final project")

library(rvest)
library(RSelenium)
library(plyr)

### Section 1: RSelenium ###

# Enter the gene to search
gene <- "CHD2"

# From https://stackoverflow.com/questions/42468831/how-to-set-up-rselenium-for-r
# This runs a selenium server with chrome browser, wait for necessary files to download
rD <- rsDriver()

# puts a variable to the session
remDr <- rD$client

# Search Exac

# URL for Exac
exac_url <- "http://exac.broadinstitute.org/"

# This goes to the exac website
remDr$navigate(exac_url)

# This enters the gene and hits enter. This takes the user to the gene site
# \uE007 simulates "enter".
webElem <-
  remDr$findElement(using = 'css selector', "#home-searchbox-input")
webElem$sendKeysToElement(list(gene, "\uE007"))

# This extracts the contents of the website
exac_html <- read_html(remDr$getPageSource()[[1]])

# This uses rvest to get the table from Selenium
exac <- exac_html %>%
  html_nodes("#variant_table") %>%
  html_table
dfExac = as.data.frame(exac)

# This makes sure a valid entry was made using an if statement.
# If a valid gene is not entered it ends the program and lets the
# user know that it was not a valid gene.
if (length(dfExac) == 0) {
  # If a valid name was not entered then the program stops with this message.
  paste("This is not a valid gene, please try again")
  
  # close Selenium
  remDr$close()
  
} else {
  # If a valid gene was entered, then the program will move to the HGMD website
  
  # Search HGMD
  
  # Username for HGMD
  username <- "joshua_husk"
  
  # Password for HGMD
  password <- "trial"
  
  # URL for HGMD
  HGMD_url <-
    "https://portal.biobase-international.com/cgi-bin/portal/login.cgi"
  
  # This goes to the HGMD website
  remDr$navigate(HGMD_url)
  
  # This enters the Username
  webElem <-
    remDr$findElement(
      using = 'css selector',
      "#login_form > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(2) > td:nth-child(1) > label:nth-child(1) > input:nth-child(1)"
    )
  webElem$sendKeysToElement(list(username))
  
  # This enters the password and hits enter
  webElem <-
    remDr$findElement(
      using = 'css selector',
      "#login_form > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(4) > td:nth-child(1) > label:nth-child(1) > input:nth-child(1)"
    )
  webElem$sendKeysToElement(list(password, "\uE007"))
  
  exac_html <- read_html(remDr$getPageSource()[[1]])
  
  
  # This section of the code checks to see if the username and password are correct
  
  # This will give 'Error! No subscription' if the subscription is expired
  checkOutput <- exac_html %>%
    html_nodes("#login_form > table > tbody > tr:nth-child(8) > td > span") %>%
    html_text()
  
  # This will give 'Error: A wrong username or password was entered.' Or 'Error:
  # Subscription has expired. 'if the wrong info was entered
  checkOutput2 <- exac_html %>%
    html_nodes("#login_form > table > tbody > tr:nth-child(8) > td > span > font") %>%
    html_text()
  
  # If password and user name are bad this will load the excel files
  # If the password and user name are good, then it will scrape the website
  
  if (checkOutput == "Error! No subscription" |
      checkOutput2 == "Error: A wrong username or password was entered. " |
      checkOutput2 == "Error: Subscription has expired.") {
    # close Selenium
    remDr$close()
    
    # This will open the CSV files after it checks if it is empty, if the files
    # are empty then it sets it to zero
    
    if (file.info(paste("dfHGMDMissense_", gene, ".csv", sep = ""))$size > 5) {
      dfHGMDMissense <- read.csv2(
        paste("dfHGMDMissense_", gene, ".csv", sep = ""),
        sep = ",",
        stringsAsFactors = FALSE
      )
    } else {
      dfHGMDMissense <- 0
    }
    
    if (file.info(paste("dfHGMDSplice_", gene, ".csv", sep = ""))$size > 5) {
      dfHGMDSplice <- read.csv2(
        paste("dfHGMDSplice_", gene, ".csv", sep = ""),
        sep = ",",
        stringsAsFactors = FALSE
      )
    } else {
      dfHGMDSplice <- 0
    }
    
    
    if (file.info(paste("dfHGMDDelete_", gene, ".csv", sep = ""))$size > 5) {
      dfHGMDDelete <- read.csv2(
        paste("dfHGMDDelete_", gene, ".csv", sep = ""),
        sep = ",",
        stringsAsFactors = FALSE
      )
    } else {
      dfHGMDDelete <- 0
    }
    
    
    if (file.info(paste("dfHGMDInsert_", gene, ".csv", sep = ""))$size > 5) {
      dfHGMDInsert <- read.csv2(
        paste("dfHGMDInsert_", gene, ".csv", sep = ""),
        sep = ",",
        stringsAsFactors = FALSE
      )
    } else {
      dfHGMDInsert <- 0
    }
    
  } else {
    # This will scrape the website
    # This finds the link to the database
    webElem <-
      remDr$findElement(
        using = 'css selector',
        "#form-template > tbody > tr:nth-child(2) > td > div > table > tbody > tr:nth-child(2) > td > a"
      )
    # click the search link
    webElem$clickElement()
    
    # An authentication window pops up, to get around this
    # I need to switch to the main window
    # get all windows
    windows <- remDr$getWindowHandles()
    
    # Swith to main window
    remDr$switchToWindow(windows[[1]])
    
    # This enters the username and password into the url
    HGMD_url2 <-
      paste(
        "http://",
        username,
        ":",
        password,
        "@hgmdtrial.biobase-international.com/hgmd/pro/start.php",
        sep = ""
      )
    
    # This goes to the HGMD website
    remDr$navigate(HGMD_url2)
    
    # Swith to other window
    remDr$switchToWindow(windows[[2]])
    
    # Close the window
    remDr$closeWindow()
    
    # Swith to main window
    remDr$switchToWindow(windows[[1]])
    
    # This finds the gene link
    webElem <-
      remDr$findElement(using = 'css selector',
                        "body > div.top > div.links > a:nth-child(1)")
    
    # click the search link
    webElem$clickElement()
    
    # This enters the gene and hits enter
    webElem <-
      remDr$findElement(
        using = 'css selector',
        'body > div.content > form:nth-child(2) > table > tbody > tr > td:nth-child(1) > input[type="text"]:nth-child(1)'
      )
    webElem$sendKeysToElement(list(gene, "\uE007"))
    
    # This finds the mutation link
    webElem <-
      remDr$findElement(
        using = 'css selector',
        'body > div.content > form > table:nth-child(5) > tbody > tr:nth-child(12) > td:nth-child(3) > input[type="submit"]:nth-child(2)'
      )
    
    # click the search link
    webElem$clickElement()
    
    # This extracts the contents of the website
    HGMD_html <- read_html(remDr$getPageSource()[[1]])
    
    # This uses rvest to get the missense table from Selenium
    HGMDMissense <- HGMD_html %>%
      html_nodes("body > div.content > table:nth-child(6)") %>%
      html_table
    dfHGMDMissense = as.data.frame(HGMDMissense)
    colnames(dfHGMDMissense)[3] <- "Consequence"
    
    # This uses rvest to get thesplicing  table from Selenium
    HGMDSplice <- HGMD_html %>%
      html_nodes("body > div.content > table:nth-child(9)") %>%
      html_table
    dfHGMDSplice = as.data.frame(HGMDSplice)
    colnames(dfHGMDSplice)[3] <- "Consequence"
    
    # This uses rvest to get thesplicing  table from Selenium
    HGMDDelete <- HGMD_html %>%
      html_nodes("body > div.content > table:nth-child(12)") %>%
      html_table
    dfHGMDDelete = as.data.frame(HGMDDelete)
    colnames(dfHGMDDelete)[3] <- "Consequence"
    
    # This uses rvest to get the inserts  table from Selenium
    HGMDInsert <- HGMD_html %>%
      html_nodes("body > div.content > table:nth-child(15)") %>%
      html_table
    dfHGMDInsert = as.data.frame(HGMDInsert)
    colnames(dfHGMDInsert)[3] <- "Consequence"
    
    # close Selenium
    remDr$close()
  }
}

### Section 2: Analysis ###
# This section removes duplicates and prints out the unique file,
# This also prints out the duplicates in a seperate file for reference

# formate the Exac data to make it easier to read and compatible with HGMD

# THe headers were stored in row one, this makes row one the header and then deletes
# the row
colnames(dfExac) <- dfExac[1, ]
dfExac <- dfExac[-1, ]

# This reformats the Variant data, there was a bunch of junk that was scrapped
# with it.
# Remove whitespace
dfExac$Variant <- gsub(" ", "", dfExac$Variant, fixed = TRUE)

# Remove extra characters
dfExac$Variant <- gsub("\n", "", dfExac$Variant, fixed = TRUE)

# This makes the consequence column compatible with Exac by removing the p.
dfExac$Consequence <-
  gsub("p.", "", dfExac$Consequence, fixed = TRUE)

# We only need data that has an entry in the Consequence column, any blank data
# can be removed, to do this, NA is added to the Consequence column and then all
# NA's are removed.
dfExac$Consequence[dfExac$Consequence == ""] <- NA
dfExac_sub <- na.omit(dfExac)

# Compare the data

# This makes sure there is data in the dataframe before anlaying,
# If there is no data it prints a statem that there is no data
# and moves to the next data set.
if (exists("dfHGMDMissense") && is.data.frame(get("dfHGMDMissense")) == 'FALSE') {
  paste("There is no data to analyze for HGMD Missense")
} else {
  # Removing duplicates for Missens
  
  # This combines the data frames by using plyr
  df1_mis <- join(dfHGMDMissense, dfExac_sub, type = "full")
  
  # This removes the duplicates
  df1_mis <-
    df1_mis[!(duplicated(df1_mis$Consequence) |
                duplicated(df1_mis$Consequence, fromLast = TRUE)), ]
  
  # This keeps the rows needed
  keep <-
    c(
      "Consequence",
      "HGMD.codonchange",
      "HGVS.nucleotide.",
      "HGVS.protein.",
      "Variantclass",
      "Reported.phenotype"
    )
  df2_mis <- df1_mis[keep]
  
  # This omits all na's and gives the final value with no duplicates
  dfHGMDMissense_final <- na.omit(df2_mis)

  
  # For reference, this will get all of the duplicates
  # This collects the duplicates
  dfHGMDMissense_Duplicate <-
    merge(dfExac_sub, dfHGMDMissense, by = "Consequence", all = FALSE)
}



# This makes sure there is data in the dataframe before anlaying,
# If there is no data it prints a statem that there is no data
# and moves to the next data set.
if (exists("dfHGMDSplice") && is.data.frame(get("dfHGMDSplice")) == 'FALSE') {
  paste("There is no data to analyze for HGMD Splices")
} else {
  # Removing duplicates for Splice
  
  # This combines the data frames
  df1_spl <- join(dfHGMDSplice, dfExac_sub, type = "full")
  
  # This removes the duplicates
  df1_spl <-
    df1_spl[!(duplicated(df1_spl$Consequence) |
                duplicated(df1_spl$Consequence, fromLast = TRUE)), ]
  
  # This keeps the rows needed
  keep <-
    c("Consequence",
      "HGMD.splicing.mutation",
      "Variantclass",
      "Reported.phenotype")
  df2_spl <- df1_spl[keep]
  
  # This omits all na's and gives the final value with no duplicates
  dfHGMDSplice_final <- na.omit(df2_spl)

  
  # For reference, this will get all of the duplicates
  # This collects the duplicates
  dfHGMDSplice_Duplicate <-
    merge(dfExac_sub, dfHGMDSplice, by = "Consequence", all = FALSE)
}


# This makes sure there is data in the dataframe before anlaying,
# If there is no data it prints a statem that there is no data
# and moves to the next data set.
if (exists("dfHGMDDelete") && is.data.frame(get("dfHGMDDelete")) == 'FALSE') {
  paste("There is no data to analyze for HGMD Deletions")
} else {
  # Removing duplicates for Deletions
  # This combines the data frames
  df1_del <- join(dfHGMDDelete, dfExac_sub, type = "full")
  
  # This removes the duplicates
  df1_del <-
    df1_del[!(duplicated(df1_del$Consequence) |
                duplicated(df1_del$Consequence, fromLast = TRUE)), ]
  
  # This keeps the rows needed
  keep <-
    c("Consequence",
      "HGMD.deletion",
      "Variantclass",
      "Reported.phenotype")
  df2_del <- df1_del[keep]
  
  # This omits all na's and gives the final value with no duplicates
  dfHGMDDelete_final <- na.omit(df2_del)
  
  
  # For reference, this will get all of the duplicates
  # This collects the duplicates
  dfHGMDDelete_Duplicate <-
    merge(dfExac_sub, dfHGMDDelete, by = "Consequence", all = FALSE)
}


# This makes sure there is data in the dataframe before anlaying,
# If there is no data it prints a statem that there is no data
# and moves to the next data set.
if (exists("dfHGMDInsert") && is.data.frame(get("dfHGMDInsert")) == 'FALSE') {
  paste("There is no data to analyze for HGMD Insertions")
} else {
  # Removing duplicates for Inserts
  
  # This combines the data frames
  df1_ins <- join(dfHGMDInsert, dfExac_sub, type = "full")
  
  # This removes the duplicates
  df1_ins <-
    df1_ins[!(
      duplicated(df1_ins$Consequence) |
        duplicated(df1_ins$Consequence, fromLast = TRUE)
    ), ]
  
  # This keeps the rows needed
  keep <-
    c("Consequence",
      "HGMD.insertion",
      "Variantclass",
      "Reported.phenotype")
  df2_ins <- df1_ins[keep]
  
  # This omits all na's and gives the final value with no duplicates
  dfHGMDInsert_final <- na.omit(df2_ins)
  
  
  # For reference, this will get all of the duplicates
  # This collects the duplicates
  dfHGMDInsert_Duplicate <-
    merge(dfExac_sub, dfHGMDInsert, by = "Consequence", all = FALSE)
}

# This merges all non-duplicate data
dfHGMD_final <- join(dfHGMDMissense_final, dfHGMDSplice_final, type = "full")
dfHGMD_final <- join(dfHGMD_final, dfHGMDDelete_final, type = "full")
dfHGMD_final <- join(dfHGMD_final, dfHGMDInsert_final, type = "full")

# This checks to see if there are non-duplicates to write to a file
rowIns_final <- nrow(dfHGMD_final)

# This prints to a csv file if there are non-duplicate.
if (rowIns_final > 0) {
  write.csv(
    dfHGMD_final,
    file = paste("dfHGMD_final_", gene, ".csv", sep = ""),
    row.names = FALSE
  )
}


# This merges all duplicate data
dfHGMD_Duplicate <- join(dfHGMDMissense_Duplicate, dfHGMDSplice_Duplicate, type = "full")
dfHGMD_Duplicate <- join(dfHGMD_Duplicate, dfHGMDDelete_Duplicate, type = "full")
dfHGMD_Duplicate <- join(dfHGMD_Duplicate, dfHGMDInsert_Duplicate, type = "full")

# This checks to see if there are duplicates to write to a file
rowIns_Duplicate <- nrow(dfHGMD_Duplicate)

# This prints to a csv file if there are duplicates.
if (rowIns_Duplicate > 0) {
  write.csv(
    dfHGMD_Duplicate,
    file = paste("dfHGMD_Duplicate_", gene, ".csv", sep = ""),
    row.names = FALSE
  )
}

