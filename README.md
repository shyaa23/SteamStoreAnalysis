# STA 518 Team Project

This repository was created for [Grand Valley State University](http://www.gvsu.edu) course STA 518, *Statistical Computing and Graphics with R*, as a part of the final project. The goal of this project was to explore the Steam videogame library and determine what makes a popular and successful game. 

## Getting Started 

This repository was created using GitHub in conjuction with RStudio Cloud and should consist of the following files:

* README.md
* team-project-instructions.md
* [team-project-report.Rmd](https://github.com/sta518/team-project-s02-blue/blob/master/team-project-report.Rmd)
* [SteamAnalyzer Shiny App](https://github.com/sta518/team-project-s02-blue/tree/master/SteamAnalyzer)
* [data-prep.R](https://github.com/sta518/team-project-s02-blue/blob/master/data-prep.R)
* [web-scrape.R](https://github.com/sta518/team-project-s02-blue/blob/master/web-scrape.R)
* [data folder](https://github.com/sta518/team-project-s02-blue/tree/master/SteamAnalyzer/data)

### team-project-instructions.md

This file contains the instruction for the team project assignment and was provided by the course instructor @dykesb. 

### team-project-report.Rmd

This file contains our project report detailing our goal to investigate what elements are most important to making a successful games on Steam, a video game digital distribution service platform including the development of the Shiny App.

### data-prep.R

This file provides the code used to clean the data to create the `steam_final.csv` data. 

### web-scrape.R

This file provides the code used to pull information from IMDB to create the `topTV.csv` data. 

### SteamAnalyzer Shiny App

This folder contains the three components of our shiny app - a global file, a ui file, and a server file which run together to create the [Steam Analyzer Shiny App]().

### Data Folder

The data folder contains the data file, `steam.csv`, with the data provided form Kaggle. The dataset contains 27,075 observations, and 18 variables. We also provide a cleaned version under the name `steam_final.csv`. The Top 100 TV show data scraped from [IMDB](www.imdb.com) is also provided as `topTV.csv`. 

#### Codebook

 Variable ID | Description  
---|---
appid  |  Unique identifier for each title
name | Title of app (game)
release_date  |  Release date in format YYYY-MM-DD
english |  Language support: 1 if is in English
developer |  Name (or names) of developer(s). Semicolon delimited if multiple
publisher |  Name (or names) of publisher(s). Semicolon delimited if multiple
platforms |  Semicolon delimited list of supported platforms. At most includes: windows;mac;linux
required_age  |  Minimum required age according to PEGI UK standards. Many with 0 are unrated or unsupplied.
categories  |  Semicolon delimited list of game categories, e.g. single-player;multi-player
genres  |  Semicolon delimited list of game genres, e.g. action;adventure
steamspy_tags |  Semicolon delimited list of top steamspy game tags, similar to genres but community voted, e.g. action;adventure
achievements  |  Number of in-games achievements, if any
positive_ratings  |  Number of positive ratings, from SteamSpy
negative_ratings  |  Number of negative ratings, from SteamSpy
average_playtime  |  Average user playtime, from SteamSpy
median_playtime   | Median user playtime, from SteamSpy
owners  |  Estimated number of owners. Contains lower and upper bound (like 20000-50000). May wish to take mid-point or lower bound. Included both to give options.
price | Current full price of title in GBP, (pounds sterling)

## Acknowledgments

* PurpleBooth's [README Template](https://gist.github.com/PurpleBooth/109311bb0361f32d87a2) was referenced in the creation of this README file.
