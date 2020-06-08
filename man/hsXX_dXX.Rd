\name{hsXX_dXX}
\alias{hsXX_dXX}
\docType{data}

\alias{hs01_d00}
\alias{hs02_d00}
\alias{hs02_d10}
\alias{hs04_d00}
\alias{hs05_d00}
\alias{hs06_d00}
\alias{hs06_d00}
\alias{hs08_d00}
\alias{hs08_d10}
\alias{hs09_d00}
\alias{hs10_d00}
\alias{hs11_d00}
\alias{hs12_d00}
\alias{hs13_d00}
\alias{hs15_d00}
\alias{hs16_d00}
\alias{hs17_d00}
\alias{hs18_d00}
\alias{hs19_d00}
\alias{hs20_d00}
\alias{hs21_d00}
\alias{hs22_d00}
\alias{hs23_d00}
\alias{hs24_d00}
\alias{hs25_d00}
\alias{hs26_d00}
\alias{hs27_d00}
\alias{hs28_d00}
\alias{hs29_d00}
\alias{hs30_d00}
\alias{hs31_d00}
\alias{hs32_d00}
\alias{hs33_d00}
\alias{hs34_d00}
\alias{hs35_d00}
\alias{hs36_d00}
\alias{hs37_d00}
\alias{hs38_d00}
\alias{hs39_d00}
\alias{hs40_d00}
\alias{hs41_d00}
\alias{hs42_d00}
\alias{hs44_d00}
\alias{hs45_d00}
\alias{hs46_d00}
\alias{hs47_d00}
\alias{hs48_d00}
\alias{hs49_d00}
\alias{hs50_d00}
\alias{hs51_d00}
\alias{hs51_d10}
\alias{hs52_d00}
\alias{hs53_d00}
\alias{hs54_d00}
\alias{hs55_d00}
\alias{hs56_d00}
\title{
   U. S. Health Service Area Boundary Spatial Polygons datasets for U. S. States 
   and the District of Columbia for the 2000 and 2010 Census
}
\description{
This document describes the content and format of the 942 health service area
boundary .rda datasets integrated in this package for 2000 and 2010 census years. 
The \var{SeerMapper} package contains a set of boundary ".rda" files
for the health service areas for all 51 states and the D. C. sub-areas for both the 2000 and 2010 
census year.    
The dataset name and object name convention used is hsXX.dYY, 
where "hs" indicates health service area (HSA) boundaries, XX is the state 
FIPS code and YY is the last 2 digits of the census year (\var{00} or \var{10}).  
Only three states have a different set of counties and boundaries: 
Colorado, Alaska, and Virginia. These states have two HSA ".rda" files 
in the package - one for each census year.  All
other states use the "00" version for both census years.

}
\usage{data(hs01_d00)}
\format{
    Each dataset contains a SpatialPolygons structure representing the 
    health service area boundaries in a single state.  
    Each "polygons" item in the structure represents 
    one HSA and all of the polygons required to define the HSA, 
    including lakes, holes, etc. The HSA boundary files are based 
    on the census tract boundaries used in the 2000 and 2010 Census.
    The coordinates system used in these rda files have been 
    transformed from the census Long/Lat coordinates values using NAD83
    to an Alber equal area projections for mapping.
}
\details{
    Each dataset (.rda file) represents the boundaries for the HSAs 
    in one state for the 2000 and 2010 U. S. census.  
    In the 2010 census the counties in three states, Alaska, Colorado, 
    and Virginia changed in size and shape.  In all other cases, 
    the 2000 census county boundaries can be reused when 
    mapping 2010 location IDs. The county boundaries were used to 
    build the HSA boundaries. To handle the three exceptions,
    three additional HSA boundary datasets are included and used by the 
    package when the \var{censusYear} is set to \var{2010} and 
    HSAs in these states are referenced in the data (at the HSA, 
    county or tract level.)
    
    Based on the locations on the rate or category data 
    provided by the user, only needed HSA boundary information 
    is loaded by the package for the referenced states. 
    The multiple state boundary data maybe accumulated 
    by the package and used for mapping.

    The data formats are the same for each HSA boundry .rda 
    in this package, a single SpatialPolygons variable.  
    The format of the dataset names is "hs", the two digit 
    state FIPS code, "_d", the two digit census year (00 or 10), 
    and ".rda".  The \var{SeerMapper} package contains the 
    set of boundary datasets for all of the U. S. 2000 Census 
    HSAs as individual datasets named hsXX_d00 and the three
    additional boundary datasets for the three states 
    where the counties changed in the 2010 census.  
    
    Each row.name of the SpatialPolygons is a polygons slot 
    in the data structure and represents a set of Polygons 
    that form the HSA.  The row.names used are the 
    the 3 digit HSA numbers assigned by NIH.  
    
    The HSA information that may normally be carried as a 
    data.frame with a SpatialPolygonsDataFrame is stored 
    in the \var{hs99_d00} dataset in the package.
    This allows the package to all of the county information 
    without having to load the boundary data.
    
    The HSA boundary information has been transformed from 
    the U. S. Census boundary data's lat/long coordinates under NAD83
    to the Alber equal area cartisian coordinates.
    
    The PROJ.4 projection used for transformation of these boundaries is:
    
    "+proj=aea +lat_1=33 +lat_2=49 +lat_0=39 +lon_0=96W +ellps=WGS84"

    All boundary data used by \var{SeerMapper} were transformed 
    to this projection when the package is constructed.
    
    All the ".rda" files are compressed using the "xy" method to reduce the 
    disk space requirements.

    All of the boundary shape file data was imported from 
    the U. S. Census website on July 3, 2016 for the 2000 
    and 2010 census, processed by the url{http://www.MapShaper.org} 
    website version 01.15 using modified Visvalingam/weighted 
    method with intersection repair enabled,prevent shape removal enabled, 
    and a coordinate precision value of 0.0 to simplify the 
    boundaries from 100% to 13%.  This process reduced the memory 
    and disk space requirements by 90% while maintaining 
    a visually usable boundary image and edge alignments.  
    The simplified boundaries were then downloaded and 
    converted to SpatialPolygons variables for use by \var{SeerMapper}
    and transformed to the Alber equal area projection.
    The initial files were census tract boundaries.  These files were
    aggregated into county boundaries.  The county boundaries were
    aggregated into the HSA boundaries, Seer Registry, and State boundaries.
    The State boundaries were aggreated into the regional boundaries.
    
}

\source{
   The HSA boundary files were obtained from the Census.gov website for the 
   2000 and 2010 year census under the Map & Data category, cartagraphic boundaries. 
   The census tract shapefiles used to create these 
   boundaries were downlaoded for the U. S. Census
   website URL{http://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html} 
   on July 3, 2016.  The census tract boundaries were aggregated into county 
   boundaries.  The county boundaries were aggregated into the HSA, Registry, and 
   State boundaries.
}
\keyword{Census2000}
\keyword{Census2010}
\keyword{datasets}
\keyword{HSA}
\keyword{Seer}
\keyword{Health Service Areas}
\keyword{U. S.}
