\name{coXX_dXX}
\alias{coXX_dXX}
\docType{data}

\alias{co01_d00}
\alias{co02_d00}
\alias{co02_d10}
\alias{co04_d00}
\alias{co05_d00}
\alias{co06_d00}
\alias{co06_d00}
\alias{co08_d00}
\alias{co08_d10}
\alias{co09_d00}
\alias{co10_d00}
\alias{co11_d00}
\alias{co12_d00}
\alias{co13_d00}
\alias{co15_d00}
\alias{co16_d00}
\alias{co17_d00}
\alias{co18_d00}
\alias{co19_d00}
\alias{co20_d00}
\alias{co21_d00}
\alias{co22_d00}
\alias{co23_d00}
\alias{co24_d00}
\alias{co25_d00}
\alias{co26_d00}
\alias{co27_d00}
\alias{co28_d00}
\alias{co29_d00}
\alias{co30_d00}
\alias{co31_d00}
\alias{co32_d00}
\alias{co33_d00}
\alias{co34_d00}
\alias{co35_d00}
\alias{co36_d00}
\alias{co37_d00}
\alias{co38_d00}
\alias{co39_d00}
\alias{co40_d00}
\alias{co41_d00}
\alias{co42_d00}
\alias{co44_d00}
\alias{co45_d00}
\alias{co46_d00}
\alias{co47_d00}
\alias{co48_d00}
\alias{co49_d00}
\alias{co50_d00}
\alias{co51_d00}
\alias{co51_d10}
\alias{co52_d00}
\alias{co53_d00}
\alias{co54_d00}
\alias{co55_d00}
\alias{co56_d00}
\alias{co72_d00}
\title{
   U. S. State County Boundary Spatial Polygons datasets for U. S. States, 
   the District of Columbia and Puerto Rico for the 2000 and 2010 Census
}
\description{
This document describes the content and format of the 52 county boundary .rda datasets 
integrated in this package for 2000 and 2010 census years. 
The \var{SeerMapper} package contains a set of boundary ".rda" files
for the 52 states', D. C.'s and Puerto Rico county or parish sub-areas for both the 2000 and 2010 
census year.  
Each dataset contains the boundary information for all of the  county/parish sub-area
in that state, district or territory.  
The dataset name and object name convention used is coXX.dYY, 
where "co" indicates county boundaries, XX is the state FIPS code and 
YY is the last 2 digits of the census year (\var{00} or \var{10}).  
Only three states have a different set of counties and boundaries: Colorado, Alaska, and Washington. 
Only these states have two county ".rda" files in the package - one for each census year.  All
other states use the "00" version for both census years.

}
\usage{data(co01_d00)}
\format{
    Each dataset contains a SpatialPolygons structure representing the county boundaries 
    for all counties in a single state.  Each "polygons" item in the structure represents 
    one county and all of the polygons required to define the county, including lakes, holes, etc.
    The boundary files are based on the boundaries used in the 2000 and 2010 Census.
    The coordinates system used in these rda files have been transformed from the census 
    Long/Lat values to an equal area projections for mapping.
}
\details{
    Each dataset (.rda file) represents the county boundaries for one state for the 
    2000 and 2010 U. S. census.  In the 2010 census the counties in three states,  
    Alaska, Colorado, and Virginia changed.  In all other cases, the 2000 census county 
    boundaries can be reused when mapping 2010 location IDs.  To handle the three exceptions,
    three additional county boundary datasets are included and used by the 
    package when the \var{censusYear} is set to \var{2010} and counties in these states are
    referenced in the data (at the county or tract level.)
    
    Based on the locations on the rate or category data provided by 
    the user, only needed county boundary information is loaded by the package for the 
    referenced states. The multiple sate boundary data maybe accumulated 
    by the package and used for mapping.

    The data formats are the same for each county boundry .rda in this package, a single
    SpatialPolygons variable.  
    The format of the dataset names is "co", the two digit state FIPS code, "_d", 
    the two digit census year (00 or 10), and ".rda".     
    The \var{SeerMapper} package contains the set of boundary datasets for all of  
    the U. S. 2000 Census counties as individual datasets named coXX_d00 and the three
    additional boundary datasets for the three states where the counties changed in the 
    2010 census.  
    
    Each row.name of the SpatialPolygons is a polygons slot in the data structure 
    and represents a set of Polygons that form the county.  The row.names used are the 
    the 5 digit FIPS state/county code associated with the county.  
    
    The county information that may normally be carried as a data.frame with a 
    SpatialPolygonsDataFrame is stored in the \var{co99_d00} dataset in the package.
    This allows the package to all of the county information without having to load
    the boundary data.
    
    The county boundary information has been transformed from 
    the U. S. Census boundary data's lat/long coordinates under NAD83
    to the Alber equal area cartisian coordinates.
        
    The PROJ.4 projection used for transformation of these boundaries is:
        
    "+proj=aea +lat_1=33 +lat_2=49 +lat_0=39 +lon_0=96W +ellps=WGS84"
    
    All boundary data used by \var{SeerMapper} were transformed 
    to this projection when the package is constructed.
        
    All the ".rda" files are compressed using the "xy" method to reduce the 
    disk space requirements.

    All of the boundary shape file data was imported from the U. S. Census website
    on July 3, 2016, processed by the url{http://www.MapShaper.org} website 
    version 01.15 using modified Visvalingam method with intersection repair enabled, 
    prevent shape removal enabled, and a coordinate precision value of 0.0 to simplify
    the boundaries from 100% to 13%.  This process reduced the memory and disk 
    space requirements by 90% while maintaining a visually usable boundary image 
    and edge alignments.  The simplified boundaries were then downloaded and 
    converted to SpatialPolygons variables for use by \var{SeerMapper}.
}

\source{
   The county boundary files were obtained from the Census.gov website for the 
   2000 and 2010 year census under the Map & Data category, cartagraphic boundaries. 
   The shapefiles used to create these boundaries were downlaoded for the U. S. Census
   website URL{http://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html} 
   on July 3, 2016.
}
\keyword{Census2000}
\keyword{Census2010}
\keyword{datasets}
\keyword{U. S.}
