\name{state_DemYY_XX_Data}
\alias{state_DemYY_XX_Data}
\alias{GA_Dem_Co_Data}
\alias{GA_Dem_Tr_Data}
\alias{WA_Dem_Co_Data}
\alias{WA_Dem_Tr_Data}
\alias{GA_Dem10_Co_Data}
\alias{WA_Dem10_Co_Data}
\docType{data}
\title{state 2000 or 2010 County / Census Tract level Test demographic data}
\description{
   These datasets are based on the 2000 and 2010 census demographic data 
   for all of the counties or census tracts in the identified state.
   This data supports the examples in the \var{SeerMapper} package
   and contains demographics for Georgia (GA) and Washington (WA) state.
   Each dataset contsins the location IDs (either a county FIPS codes (5 digits) or census tract FIPS 
   code (11 digits)), the abbreviation of the associated Seer Registry (if any),
   2000/2010 population, area in sq. miles of each county or tract,
   2000/2010 population density (pop/area),
   population 65 years old or older,
   the number of housing units, the number of units occupied,
   the number of units occupied by owner, and population of hispanics.
   
   In Georgia all counties and census tracts belong to registries.
   The three Seer registry areas in Georgia are identifed, using the abbreviations of:
   GA-ATL, GA-RUR, and GA-OTH.  In the examples, this data is used for full state mapping, 
   Seer registry areas mapping (subdivided by Seer registry area abbreviations), and 
   randomly selected data to represent different situations for partial 
   data collections.  This data can be used by both the \var{SeerMapper} package,
}
\usage{data(GA_Dem_Tr_Data)}
\format{
  The naming convention for the files are:  SS_DemYY_AA_Data, where SS is the same 2 character
  postal abbreviation (GA = Georgia and WA = Washingon), YY is "" for 2000 and "10" for 2010 
  census demographic data, and AA is the area type (Co = County, and Tr = Census Tract).
  The files contained in this package are:
  \itemize{
   \item GA_Dem_Co_Data - Georgia County Demographic data for 2000
   \item GA_Dem10_Co_Data - Georgia County Demographic data for 2010
   \item GA_Dem_Tr_Data - Georgia Census Tract Demographic data for 2000
   \item WA_Dem_Co_Data - Washington County Demographic data for 2000
   \item WA_Dem10_Co_Data - Washington County Demographic data for 2010
   \item WA_Dem_Tr_Data - Washington Census Tract Demographic data for 2000
  }
  The U. S. Census demographic data files contains hundreds for variables and counts.  These
  files are used in the examples and only contain the following 11 columns of data:
  \describe{
   \item{FIPS}{is a character vector of the U.S. County (5 digits) or Census Tract (11 digits) FIPS codes.
    }
   \item{pop2000 or pop2010}{is a integer containing the 2000 or 2010 sub-area population.
    }
   \item{popdens}{is a integer containing the 2000 or 2010 population density of the sub-areat.
    }
   \item{areasm}{is a numeric area of the sub-area( county or tract) in square miles.
    }
   \item{saID}{is a character factor vector of the abbreviation of the Seer Registry
      the sub-area is a member.  The Seer Registry abbreviations match the 
      abbreviations used in the package boundaries datasets for each Seer Registry 
      state.  For Georgia the three Seer Registries are: GA-ATL, GA-RUR, and GA-OTH.
    }
   \item{age.65.up}{is an integer representing the population with the age of 65 or higher.
    }
   \item{hh.units}{is an integer representing the number of physical housing units
      in the county or census tract.
    }
   \item{hh.occupied}{is an integer representing the number of housing units that are
      occupied.  (hh.units-hh.occupied) equals the number of housing units vacant.
    }
   \item{hh.owner}{is an integer representing the number of housing units that are 
      occupied by the owner.  (hh.occupied-hh.owner) equals the number of 
      units that are occupied by renters.
   }
  }
}
\source{
The Georgia and Washington 2000 and 2010 Census demographis at the county and census tract levels
are based on the American Fact Finder data from the CENSUS.GOV website demographic data and the   
the R package UScensus2000tract package.
}
\keyword{datasets}
\keyword{Census2000}
\keyword{Census2010}

