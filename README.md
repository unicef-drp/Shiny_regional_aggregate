# Shiny app to show regional aggregate

Based on the BWC aggragating method, the app wraps the original `outputaggregates-BWC` code.
Use the script `update_me_every_year.R` to update every year.

Deployed here: https://unicef-dapm.shinyapps.io/Shiny_regional_aggregate/

To run the code locally, download the repo and launch it as R project using the __`Rporj`__ file,
Or just open the `app.R` and run.
```{r}
git clone https://github.com/UnicefDAPM/Shiny_regional_aggregate.git
```
It is also on Dropbox under the folder `Shiny202x`. 

To run the app directly from github (not working now since the repo is private)
```{r}
shiny::runGitHub("unicef-drp/Shiny_regional_aggregate.git", "liuyanguu")
```
## How to update
Please use `update_me_every_year.R` to walk through the process.  
The dataset to be updated are described in the script too.

## Major update  
January 2020  
- Allow uploading a list of ISO countries as countries to be selected,
- Allow renaming the new selected group of countries (default as "selected countries")

March 2022
- Add age groups for older children (incl. adolescents)
