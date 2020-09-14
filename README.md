# Shiny app to show regional aggregate

Based on the BWC aggragating method, the app wraps the original `outputaggregates` code.

To run the app directly from github (access to this group is required, so not applicable for most users):
```{r}
shiny::runGitHub( "unicef-drp/Shiny_regional_aggregate", "liuyanguu")
```

To run the code locally, download the repo and launch it as R project using the __`Rporj`__ file.
```{r}
git clone https://github.com/UnicefDAPM/Shiny_regional_aggregate.git
```

It is also on Dropbox under folder `Shiny2019`. 

## Updates  
* January 2020  
- Allow uploading a list of ISO countries as selected countries,
- Allow renaming the new selected group of countries (default as "selected countries")
