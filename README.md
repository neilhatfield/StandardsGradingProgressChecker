# Standards Grading Progress Checker

The intent with this repo is to provide individuals with a template for a Shiny
app that they can share with their students to help them track their progress
throughout the semester.

## Key R Packages

There are several packages that are needed for my formulation of this app; they
include:

+ Shiny app packages
  + `shiny`
  + `shinydashboard`
  + `shinyBS`
  + `shinyjs`
  + `shinyWidgets`
+ Security
  + `openssl`
  + `AzureAuth`
    - My institution uses Microsoft Azure for logins; yours may use something else
    - You will need permissions from your institution to be able to link into the authentication system
+ Data Cleaning and Graphics
  + `tidyverse`
  + `fmsb`
  + `colorspace`
+ Additional Shiny Controls and Styles
  + [`boastUtils`](https://github.com/EducationShinyAppTeam/boastUtils)

## General Directory Structure and Files

For this app, I've set up the general directory structure as follows:

+ app.R (This contains all necessary code for the shiny app)
+ .Renviron (This contains security and authentication information; you'll need to create this)
+ README.md and DESCRIPTION
+ dataFiles Directory
  - Encrypted data frames (attendance and gradebook)
  - login list with classifications
    - Classifications include student, special (for app development), and teacher
  - list of learning outcomes and objectives
+ localOnly Directory (does not get loaded to your Shiny server)
  - gradebookPrep.R (contains the scripts you would use to encrypt data)
  - Un-encrypted files

