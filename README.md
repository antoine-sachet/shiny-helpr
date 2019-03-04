## Shiny helper

This package defines the `navbarLogoPage` which is an improved `navbarPage` with

* a logo (top left) in addition to the title
* an optional favicon
* integration with shinyproxy: the navbar contains links to home, links to sign out and links to the shinyproxy admin panel.

## Integration with shinyproxy

The shinyproxy navbar is now redundant since the app navbar provides all the required links. 
You can use `hide-navbar: TRUE` in the shinyproxy `application.yml` to hide it.


The `shinyproxy` argument to navbarLogoPage can be set to `TRUE`, `FALSE` or `"auto"`. 
If the latter, whether the app is running within shinyproxy is detected by checking the environment variables 
for SHINYPROXY_USERNAME and SHINYPROXY_USERGROUPS, which are automatically set by shinyproxy when booting an app.

## Installation

To install the latest development builds directly from GitHub, run this instead:

```r
if (!require("devtools"))
  install.packages("devtools")
devtools::install_github("asachet/shiny-helpr")
```


## License

This package is based on code from Shiny and is therefore licensed under GPL-3. 

The shiny package as a whole is licensed under the GPLv3. See the [LICENSE](LICENSE) file for more details.
