# Shiny app to show regional aggregate

## D&A - Child Mortality Group 
A prototype started on Oct.7th, 2019.
Based on the new BWC aggragating method, the app simply wraps the original code.
Based on the output of 2019.09 Child Mortality Report

To run the app directly from github (access to this group is required, so not applicable for most users):
```{r}
shiny::runGitHub( "unicef-drp/Shiny_regional_aggregate", "liuyanguu")
```

To run the code locally, clone the repo and launch the R project using the __`Rporj`__ file.
```{r}
git clone https://github.com/UnicefDAPM/Shiny_regional_aggregate.git
```

It is also on Dropbox under folder `Shiny2019`. 

## Update January 2020
- The current version allows uploading a list of ISO countries as selected countries,
- allows rename the new group (default is "selected countries")
