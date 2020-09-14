# Shiny app to show regional aggregate

Based on the BWC aggragating method, the app wraps the original `outputaggregates-BWC` code.
Update the script `update_me_every_year.R` to update yearly estimates.

To run the app directly from github (access to this group is required):
```{r}
shiny::runGitHub( "unicef-drp/Shiny_regional_aggregate", "liuyanguu")
```

To run the code locally, download the repo and launch it as R project using the __`Rporj`__ file,
Or just run the `app.R`.
```{r}
git clone https://github.com/UnicefDAPM/Shiny_regional_aggregate.git
```

It is also on Dropbox under folder `Shiny2019`. 

## Updates  
* January 2020  
- Allow uploading a list of ISO countries as selected countries,
- Allow renaming the new selected group of countries (default as "selected countries")
