\name{SeerMapper-package}
\alias{SeerMapper-package}
\docType{package}
\title{A graphics package to easily and quickly create U. S. maps at the state, 
Seer registry areas, county or census tract levels}
\description{
 The \var{SeerMapper} package provide a means of creating U.S. maps of categorized 
 data on at the state, state/county, state/county/census tract or U.S. Seer registry levels.
 The \var{SeerMapper} package contain the R script to create the maps and 
 the U. S. Census boundaries files for States, District of Columbia, and Puerto Rico, 
 the counties for each state/territory, the census tracts for each state and NCI Seer Registries.
 The main package contains the code and the census year 2000 and 2010
 state, registry and county boundary data and information. The census tract boundary data 
 could not fit into the package and stay within the CRAN 5 megabyte package size limitation.
 Six(6) data only packages released with this package contain \var{SeerMapper}'s simplified 
 U. S. 2000 and 2010 census tract boundary data: \var{SeerMapperRegs} and \var{SeerMapper2010Regs} 
 packages containing the census tracts for 
 any state with a Seer Registry for years 2000 and 2010, \var{SeerMapperEast} and \var{SeerMapper2010East}
 containing the census tracts for states east of the Mississippi river that don't have
 registries for years 2000 and 2010, and \var{SeerMapperWest} and \var{SeerMapperWest}
 containing the census tract boundary data for states without registries weat of the 
 Mississippi for years 2000 and 2010. 
 When \var{SeerMapper} is installed, the other six(6) packages are also installed.
 When \var{SeerMapper} is executing, it manages the loading of the six packages based on 
 the level of map being requested by the caller.
 
 To map at the state level, data must use the 2 digit U. S. state fips codes 
 as the location identifiers.  
 
 To map at the HSA level, data must use the 3 digit HSA numbers as the location 
 identifiers.
 
 To map at the county level, data must use the 5 digit U. S. state/county 
 fips codes as the location identifiers.  
 
 To map at the census tract level, data must use the 11 digit U. S. 
 state/county/tract fips codes as the location identifiers.   
 
 To map at the U. S. NCI Seer registry area level, the location identifier must be 
 the Seer Registry abbreviation (see documentation on the Seer registry boundary files
 for more details.)  The basic packages only contain the census tract boundaries
 for the states which contain Seer Registries.  
 
 The goal was to allow 
 a user the simpliest way to create maps of data and rates in the states, counties, 
 Seer Registries, and census tracts in states with Seer Registries using one package for 
 one census year.  If additional states required census tract mapping, this is not 
 precluded, just requires a little more setup work.  Because of the numerous
 changes in the fips codes, census tracts boundaries and even county boundaries between
 2000 and 2010, it was necessary to keep the boundary datasets separate.  
 The result is two sets of packages.
 Make sure the location identifiers in your data match census year of the 
 the base package your are using.  If mis-matches are detected, warnings are 
 generated document the missing or additional areas to help the user 
 handle the situation.  As much of the map is still generated as possible.
 
 Call options are available to help manage: the inclusion, sizing and placement of the legend,  
 hatching of areas, limiting mapping to only the contiguous 48 states, 
 and the categorizing and coloring of areas based on the data.
 
 The package will color the areas with data based on a user provided set of break points
 or on a number of categories desired.  See the \option{categ} call parameter documentation 
 for more details.
 
 A hatching option controls how the package will overlay hatching of areas 
 based on a critria and value. The default setting handle P Values in the range of 0 to 1,
 with a test value of 0.05 and areas hatched if their value is greater than the test value (>).
 See the documentation on the \option{hatch} call parameter for more details.  
 
 All 50 states, District of Columbia and Puerto Rico can be mapped using FIPS codes.  
 
 The boundaries for Alaska and Hawaii have been adjusted and scaled be provide 
 a reasonable presentation with the mainland and relocated to just south of 
 California and Arizona.  

 }
\details{
    \tabular{ll}{
      Package:  \tab SeerMapper\cr
      Type:     \tab Package\cr
      Version:  \tab 1.2.4\cr
      Date:     \tab 2020-06-22\cr
      License:  \tab GPL (>=2)\cr
      LazyLoad: \tab no\cr
    } 

 The function accepts a data.frame containing an area/location identifier, 
 data (rates, data, or categories) for the areas
 and an hatching data. The hatching data is generaly a p_value for the data (rates), 
 but can be any value.  If the data is not a p_value, then the user must 
 provide the test value and the test operation to determine if hatch is 
 required.
 Each row represents a State, State/County, State/County/Census Tract, or U.S. Seer registry area.  
 The data(rates) are categorized by default into 5 categories.  The breakpoints
 for the categories are calculated based on the quartiles for the data.
 The caller can specify the categorization be done on from 3 to 11 categories.
 The caller can also specify the specific breakpoints to use in the 
 categorization of the data via a numeric vector like c(0.6, 0.8, 1.0, 1.2, 1.4)
 for up to 10 breakpoints.  The maximum number of categories is 
 set by the color palette selected from the RColorBrewer palettes.
 The default palColors setting is the "RdYlBu" palette which can support 
 up to 11 categories.  The default \option{categ} value is \var{5}.
 
 If hatching data is provided, the default is to treat the data as p_values. 
 When hatching is enabled (\option{hatch}=TRUE), the function will overlay a hatch pattern 
 on any area with a p_value >= 0.05.  This can be changed using the 
 \var{ops} and \var{value} settings in \option{hatch} list call parameter. 
 
 If the data is for states, state/counties, state/county/census traces, 
 the area's (location) identify must be the U.S. FIPS codes. 
 If the data is for U.S. Seer registry areas, the area identify is the 
 Seer registry area abbreviation (See the \var{st99_d00} dataset documenation for more details.)
 
 The boundary dataset for the Seer registry area is contained in this 
 package as the 2000 Census.  

 The typical call sequence is:
 \preformatted{
   library(SeerMapper)
   library(maps)
 
   # create data data.frame
   
   rateData <- data.frame(Fips=state.fips$fips, 
                          RateRatio=stateRateVector, 
                          pValue=stateRatePValue)
   
   SeerMapper(rateData, Title=c("Test Map"))
  }
  
 To map census tracts in state without Seer Registries, the call sequence is:
 \preformatted{
   library(SeerMapper)
   library(maps)
 
   # create data data.frame
   
   rateData <- data.frame(Fips=cenTract.fips, 
                          RateRatio=ctRateVector, 
                          pValue=ctRatePValue)
   
   SeerMapper(rateData, Title=c("Test Non-Seer State Census Tract Map"))
  }
 
The data passed to the function will dictate which areas of the U.S. are mapped.  
In general only states with data are mapped.

INSTALLATION:

Due to CRAN R package size limitations, the base \var{SeerMapper} package could only contain the 
boundary information datasets for: the states, DC and PR boundaries,
the Seer Registry boundaries, and the boundaries for the counties in all states.
The census tract boundaries are supplied via six(6) additional R packages: \var{SeerMapperRegs}, 
\var{SeerMapperEast}, \var{SeerMapperWest}. \var{SeerMapper2010Regs}, \var{SeerMapper2010East}, 
and \var{SeerMapper2010West}.  The packages containing "2010" in their name support the census year of 2010.
The packages without "2010" support the census year of "2000".  
When \var{SeerMapper} is installed, the package dependencies have been set up to force the installation 
of the six(6) census tract package automatically. The user should not have install these packages manually.
The installation log will notify the user of the additional downloads and installations.

If the location IDs in the data cannot be associated with loaded boundary datasets, 
messages are generated to identify the packages that may need to be installed or loaded 
the library function.

The base package contains boundary information for:
\itemize{
   \item all states and DC,
   \item all Seer Registries, 
   \item all Health Service Areas (HSA) for all states and DC,   
   \item counties in all states,
}
The "Regs" boundary package contains the 2000 or 2010 census tract boundary data for 19 states: 

   Alaska, Arizona, California, Connecticut, Georgia, 
   Hawaii, Idaho, Iowa, Kentucky, Louisiana, Massachusetts, 
   Michigan, New Jersey, New Mexico, New York, Oklahoma, 
   Utah, Washington, and Wisconsin.

The "East" boundary package contains the 2000 or 2010 census tract boundary data for 20 states and DC:

   Alabama, Delaware, District of Columbia, Florida, Illinois, 
   Indiana, Maine, Maryland, Mississippi, New Hampshire, 
   North Carolina, Ohio, Pennsylvania, Rhode Island, South Carolina, 
   Tennessee, Vermont, Virginia, West Virginia, and Puerto Rico.
   
The "West" boundary package contains the 2000 or 2010 census tract boundary data for 13 states:

   Arkansas, Colorado, Kansas, Minnisota, Missouri, 
   Montana, Nebraska, Nevada, North Dakota, Oregon, 
   South Dakota, Texas, and Wyoming. 
   
To do census tract mapping, the package automatically loaded needed census tract boundary 
packages according to the census year and location IDs in data.frame provided during the call.  
If a package is missing or cannot be loaded, the caller will be given a message identifying the problem.

\preformatted{
    library(SeerMapper)
}
and all of the boundary information is will be available.
}  
\author{
James B Pearson, Jr \email{jbpearson353@gmail.com}\cr 
Maintainer: "Joe Zou" \email{zouj@imsweb.com}
}


