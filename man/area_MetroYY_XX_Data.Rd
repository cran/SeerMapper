\name{area_Metro_XX_Data}
\alias{area_Metro_XX_Data}
\alias{WashBaltMetro_Co_Data}
\alias{KCMetro_Co_Data}
\alias{KCMetro_Tr_Data}
\alias{WashBaltMetro10_Co_Data}
\alias{KCMetro10_Co_Data}
\alias{KCMetro10_Tr_Data}
\docType{data}
\title{Metro Combined Statistics Area datasets for County or census tract for Test data}
\description{
   This document described the contain of eight datasets included in the package.
   They represent demographic data at the county level and census tract level for the 
   Washington-Baltimore and Kansas City Metro Combined Statistical areas for 
   the 2000 and 2010 census years.
   Each metro area cosists of counties or tracts in multiple states.
   The Washington-Baltimore Metro CSA consists of an area that covers 5 states/districts
   (Maryland, Virginia, Pennsylvania, West Virinia, and the District of Columbia) 
   and includes 40 counties and 2178 census tracts.  The Kansas City Metro CSA
   consists of an area that covers 2 states (Kansas and Missouri) and 22 counties 
   and 604 census tracts.
}
\usage{data(WashBaltMetro_Co_Data)}
\format{
   The format of each datasets is the same. The only difference is the location 
   ID used in each data set and the number of records and census year of the data.  
   The file naming convention is: nnnnnMetroYY_AA_Data, where nnnnn is the name of the 
   metro area (KC or WashBalt), YY is "" for census year 2000 and "10" for census 
   year 2010, and AA is the type of sub-areas ("Co" = country, and "Tr" = census tract.)
   In the county datasets, the location ID is the 5 digit state/county FIPS code. 
   In the census tract datasets, the location ID is the 11 digit 
   state/county/tract FIPS code.
   
   These datasets are used to demonstrate how \var{SeerMapper} can work with  
   county or census tracts data to create maps across multiple states.
   
    The Kansas City and Washington-Baltimore Combined Statistics Area 2000 data
    at the county and census tract level contains the following demographic 
    data for each sub-area. The data contains 9 columns:
    \describe{
     \item{FIPS}{is a character vector of the U.S. State/County/Tract FIPS code (11 digits)
      }
     \item{pop2000}{is an integer containing the 2000 tract population.
      }
     \item{age.65.up}{is an integer representing the population of the people 
       with the age of 65 or higher.
      }
     \item{hh.units}{is an integer representing the number of physical household units
        in the census tract.
      }
     \item{hh.occupied}{is an integer representing the number of hh.units that are
        occupied.    (hh.units-hh.occupied) equals the number of households vacant.
      }
     \item{hh.owner}{is an integer representing the number of occupied household 
        units owned by the occupants.  (hh.occupied-hh.owner) equals the number of 
        units that are occupied by renters.
      }
     \item{areasm}{is a numeric area of the tract in square miles.
      }
     \item{popdens}{is a numeric containing the 2000 population density of the tract.
      }
  }
}
\source{
The U. S. Census Bureau 2000 and 2010 census tract demographic dataset from the 
CENSUS.GOV website demographic data files for the 2000 census as 
pulled on February, 2017.  The R package UScensus2000tract
was used to get the census tract data based on the 2001 release.  The dataset
was then aggregated to create the county dataset.
}
\keyword{datasets}
\keyword{SeerMapper}
