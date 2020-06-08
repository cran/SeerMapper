\name{state_CM_Co_Data}
\alias{state_CM_Co_Data}
\alias{Georgia_CM_Co_Data}
\alias{Washington_CM_Co_Data}
\alias{Kentucky_CM_Co_Data}
\alias{California_CM_Co_Data}
\docType{data}
\title{County Level Cancer Mortality Rates for se;ected state for 2009 to 2013}
\description{
   These datasets containing the cancer mortality
   rates from the State Cancer Profile website at the county level 
   for several states containing Seer Registries for the period of 
   2009 to 2013.  The county level data for Georgia, Washington, California, 
   and Kentucky have been included to support examples demonstrating 
   difference features of the \var{SeerMapper} package.
   It was obtained through the State Cancer Profiles website 
   at www.statecancerprofiles.cancer.gov.  This data represents
   all races and sexes for all sites of cancer.  The data was edited to 
   remove the recent trend data and to add a pseudo-pValue based on the 
   US age adjusted rate (AA_Rate = 168.5) and the abbreviation of the associated 
   Seer Registry (if any).  The dataset names for the state dataset are: Georgia_CM_Co_Data,
   Washington_CM_Co_Data, Kentucky_CM_Co_Data, and California_CM_Co_Data.
   }
\usage{data(Georgia_CM_Co_Data)}
\format{
  All of the state Cancer Mortality Rate datasets are at the county level and 
  contains 10 columns: 
  \describe{
   \item{stcoID}{is a numeric value for the state and county U. S. FIPS codes.  This is 
     used to link the data to the county and state boundary data.
     It must be a 4 or 5 character string.
    }
   \item{County}{is a character vector county name provided by State Cancer Profiles.
    }
   \item{stID}{is a character vector containing the 2 digit state FIPS code.
    }
   \item{AA_Rate}{is a numeric value of the age-adjust rate for the county.
    }
   \item{Lower_CI}{is a numeric value for the lower confidence interval 
     value for the age-adjusted rate.
    }
   \item{Upper_CI}{is a numeric value for the upper confidence interval 
     value for the age-adjusted rate.
    }
   \item{Avg_DpY}{is a numeric value for the average number of deaths per year.
     This variable is not used in this package examples.
    }
   \item{saID}{is a character factor vector of the abbreviation of the Seer Registry
      the tract is a member.  The Seer Registry abbreviations match the 
      abbreviations used in the package boundaries datasets for each Seer Registry 
      state.  For Georgia the three Seer Registries are: GA-ATL, GA-RUR, and GA-OTH.
    }
   \item{pValue}{is a numeric value of 0.02 or 0.2. The pValue is set to 0.02 if the U. S. rate
      is not within the county's confidence interval range.  It is set to 0.2 if the U. S.
      rate is within the interval.  0.02 indicates the county's rate is significant, 
      while 0.2 indicates the rate is not significant.
    }
   \item{pV}{is a logical value indicating whether the county is significant or not.
    }
  
  }
  The first two rows of the data.frame represent the rate information 
  for the state and the USA.  These rows MUST be deleted before using the data.frame
  with the package.
  
  In the Georgia and Kentucky datasets, there are two additional columns:  "FIPS" and 
  "Rate".  These columns are identical to the "stcoID" and "AA_Rate" columns and are
  used to demonstrate how the default values for the \var{idCol} and \var{dataCol} 
  call parameters operate.
}
\source{
 The State Cancer Mortality data for the years 2009 to 2013 for all races and sexes
 for each county for all cancer sites was exported from the State Cancer Profile website
 www.statecancerprofiles.cancer.gov on September 2, 2016.  The data was modified to include
 the pValue, county's Seer Registry Abbreviation (if associated), and a stripped down
 county name.  Data was collected for the counties of Georgia, California, Kentucky, 
 and Washington.
}
\keyword{datasets}
