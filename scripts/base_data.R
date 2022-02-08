#### SETUP #### ----------------------------------------------------------------
### LOAD PACKAGES --------------------------------------------------------------
library(vitae)
library(rorcid)
library(ggplot2)
library(dplyr)
library(sf)
library(rnaturalearth)
library(sfnetworks)
library(ggrepel)
library(emojifont)
library(fontawesome)
library(kableExtra)
library(huxtable)
library(gridExtra)
library(rsvg)
library(htmltools)
library(tibble)
library(here)
library(xtable)
## -----------------------------------------------------------------------------
baretable <- function(tbl, digits = 0,
                      include.colnames = FALSE, include.rownames = FALSE,
                      hline.after = NULL,
                      size = getOption("xtable.size", NULL),
                      add.to.row = getOption("xtable.add.to.row", NULL),
                      longtable = FALSE,
                      ...) {
  xtable::xtable(tbl, digits = digits, ...) %>%
    print(
      include.colnames = include.colnames,
      include.rownames = include.rownames,
      hline.after = hline.after,
      comment = FALSE,
      tabular.environment = if_else(longtable, "longtable", "tabular"),
      floating = FALSE,
      size = size,
      add.to.row = add.to.row,
      sanitize.text.function = function(x) {
        x
      }
    )
}

#### DATA #### -----------------------------------------------------------------
### EDUCATION ------------------------------------------------------------------
EDUCATION <-
  tibble::tribble(~ ID,
                  ~ UNIVERSITY,
                  ~ DATES,
                  ~ DEGREE,
                  ~ LOCATION,
                  ~ THESIS) |>
  ## PhD
  tibble::add_row(
    ID = 3,
    UNIVERSITY = "University of Tasmania",
    DATES = "2019 - 2023",
    DEGREE = "Doctor of Philosophy, Biological Sciences",
    LOCATION = "Sandy Bay, TAS, Australia",
    THESIS = c("Dissertation: Dynamic mapping and analysis of New South Wales fire regimes: Past, present, and future",
               "Supervisors: Drs. Grant J. Williamson and David M.J.S. Bowman")
  ) |>
  ## MSc
  tibble::add_row(
    ID = 2,
    UNIVERSITY = "Western Washington University",
    DATES = "2013 - 2016",
    DEGREE = "Master of Science, Geography",
    LOCATION = "Bellingham, WA, USA",
    THESIS = c("Thesis: Climatic drivers of western spruce budworm outbreaks in the Okanogan Highlands",
               "Supervisor: Dr. Aquila Flower")
  ) |>
  ## BSc
  tibble::add_row(
    ID = 1,
    UNIVERSITY = "Texas State University",
    DATES = "2007 - 2012",
    DEGREE = "Bachelor of Science, Physical Geography / Geology",
    LOCATION = "San Marcos, TX, USA",
    THESIS = "Capstone: Wildfire hazard data for the Yellowstone National Park Ecosystem: 1987 - 1988"
  )
## -----------------------------------------------------------------------------
### PROFESSIONAL EXPERIENCE ----------------------------------------------------
PROFESSIONAL_EXPERIENCE <-
  tibble::tribble(
    ~ ID,
    ~ POSITION,
    ~ DATES,
    ~ INSTITUTION,
    ~ LOCATION,
    ~ DESCRIPTION,
    ~ OUTPUT
  ) |>
  tibble::add_row(
    ID = 13,
    POSITION = "Data Analyst",
    DATES = "2021-02 - present",
    INSTITUTION = "University of Tasmania: Division of Future Students",
    LOCATION = "Sandy Bay, TAS, Australia",
    DESCRIPTION = c(
      "Developing models and tools for meeting university targets and KPIs using statistical and machine learning",
      "Managing design and implementation of broad-scale university ML models for directing future university marketing, funding, and course creation",
      "Designing and implementing programming and data analytics training seminars for institution staff",
      "Overseeing development of internal reporting and statistical packages",
      "Processing, analysing and reporting data to promote and review university programs"
    ),
    OUTPUT = "resume"
  ) |>
  tibble::add_row(
    ID = 12,
    POSITION =  "Adjunct Researcher",
    DATES = "2019-08 - present",
    INSTITUTION = "University of Tasmania: College of Sciences and Engineering",
    LOCATION = "Sandy Bay, TAS, Australia",
    DESCRIPTION = c(
      "Overseeing multi-year projct design and implementation addressing wildfire and global climate change challenges",
      "Managing custom platform for interactive web-mapping, on-the-fly statistical analyses, and novel data visualisation",
      "Developing complex data-processing and analysis pipelines using high-performance computing and big data",
      "Collaborating with national and international research teams tackling climate change risk and mitigation"
    ),
    OUTPUT = "resume"
  ) |>
  tibble::add_row(
    ID = 11,
    POSITION = "Sustainability Analyst",
    DATES = "2019-10 - 2021-02",
    INSTITUTION = "University of Tasmania: Division of Future Students",
    LOCATION = "Sandy Bay, TAS, Australia",
    DESCRIPTION = c(
      "Processed, analysed, and reported data to promote and review institution programmes",
      "Developed pipelines for automating data cleaning and collation",
      "Prepared data and briefings for global institution rankings",
      "Promoted nation-wide sustainability targets using data dashboards and reports"
    ),
    OUTPUT = "resume"
  ) |>
  ## Independent Research Contractor
  # tibble::add_row(
  #   ID = 10,
  #   POSITION = "Independent Research Contractor",
  #   DATES = "2018-07 - 2020-09",
  #   INSTITUTION = "Self-Employed",
  #   LOCATION = NA_character_,
  #   DESCRIPTION = c(
  #     "Developed large-scale data-processing methodologies for resource and hazard management",
  #     "Produced interactive, dynamic dashboard products for exploratory data analysis and modelling"
  #     # "Notable contracts incl. U.S. Forest Service (2018-08, 2019-06) and Frontier Development Lab (2020-08)"
  #   ),
  #   OUTPUT = "resume"
  # ) |>
  tibble::add_row(
    ID = 9,
    POSITION = "Data \\& GIS Analyst",
    DATES = "2018-12 - 2019-05",
    INSTITUTION = "Apex Systems: Apple, Inc.",
    LOCATION = "Austin, TX, USA",
    DESCRIPTION = c(
      "Developed and standardised data-processing methods for vector and multispectral raster datasets",
      "Managed and processed corporate GIS databases using data-driven software and programming languages",
      "Prepared statistical smmaries, models, and analytical reports",
      "Led research and development projects to define novel methodologies for geospatial data",
      "Collaboratd with a global network of analysts and technicians"
    ),
    OUTPUT = "resume"
  ) |>
  tibble::add_row(
    ID = 8,
    POSITION = "Faculty Research Assistant",
    DATES = "2017-11 - 2018-06",
    INSTITUTION = "Oregon State University: USDA Forest Service Forestry Sciences Laboratory",
    LOCATION = "Wenatchee, WA, USA",
    DESCRIPTION = c(
      "Converted complex data and findings into understandable figures and summary reports",
      "Built analytical models combining multiple statistical programming languages",
      "Created and maintained relational databases for government research",
      "Gathered and analysed ecological data using statistical software"
    ),
    OUTPUT = "resume"
  ) |>
  tibble::add_row(
    ID = 7,
    POSITION = "Biological Science Technician II",
    DATES = "2017-08 - 2017-11",
    INSTITUTION = "Washington Department of Fish and Wildlife: Region 5",
    LOCATION = "Ridgefield, WA, USA",
    DESCRIPTION = c(
      "Collected and recorded biological data for government research and both commercial and tribal fisheries",
      "Applied QA/QC to develop electronic biological data collection system and relational database",
      "Designed, built, and maintained department, fisheries, and commercial research equipment",
      "Conducted outreach to the public, providing information on fisheries research, management principles, and local regulations",
      "Analysed biological fisheries data for research reporting",
      "Monitord critical fish migrations using DNA sampling and the PTAGIS coordination project",
      "Received training in fish, plant, and organism identification, government safety procedures, boat operation, and defensive driving"
    ),
    OUTPUT = "cv"
  ) |>
  # tibble::add_row(
  #   ID = 6,
  #   POSITION = "Substitute Teacher",
  #   DATES = "2017-01 - 2017-08",
  #   INSTITUTION = "Kelso School District: Beacon Hill Elementary School",
  #   LOCATION = "Kelso, WA, USA",
  #   DESCRIPTION = c(
  #     "Taught a variety of classrooms, with a focus on K-5 primary education, special education, and LAP classrooms using specialised learning plans",
  #     "Engaged young students with an interest in scientific research, giving special outreach presentations",
  #     "Helped integrate and refine special education lesson plans as an instructional assistant"
  #   ),
  #   OUTPUT = "cv"
  # ) |>
  tibble::add_row(
    ID = 5,
    POSITION = "Graduate Assistant",
    DATES = "2013-08 - 2016-08",
    INSTITUTION = "Western Washington University: Huxley College of the Environment",
    LOCATION = "Bellingham, WA, USA",
    DESCRIPTION = c(
      "Scripted statistical methods for identifying complex ecological and climatological relationships",
      "Trained and led teams of research volunteers in the safe use of field and laboratory equipment",
      "Created and maintained university GIS geodatabses and topologies",
      "Developed and published novel dendroecological research projects",
      "Presented research findings at national conferences",
      "Presided over and maintained the Huxley Tree Ring Laboratory"
    ),
    OUTPUT = "cv"
  ) |>
  tibble::add_row(
    ID = 4,
    POSITION = "Research Engineer",
    DATES = "2013-04 - 2013-08",
    INSTITUTION = "The University of Texas at Austin: Texas Natural Science Center",
    LOCATION = "Austin, TX, USA",
    DESCRIPTION = c(
      "Compiled, georeferences, and managed academic collections and geospatial databaes",
      "Contributed historial species range data to statewide virtual biodiversity museum",
      "Reconstructed historical biodata collections and events"
    ),
    OUTPUT = "cv"
  ) |>
  tibble::add_row(
    ID = 3,
    POSITION = "Trail Management Intern",
    DATES = "2013-01 - 2013-04",
    INSTITUTION = "Student Conservation Association",
    LOCATION = "Angeles National Forest, CA, USA",
    DESCRIPTION = c(
      "Co-managed Trails Assessment and Conditions Survey (TrACS) team",
      "Conducted detailed survey of fire damage to public roads and trail systems",
      "Assisted in infrastructure restoration projects to maximise public access benefits",
      "Received national government-sponsored training in defensive driving"
    ),
    OUTPUT = "cv"
  ) |>
  tibble::add_row(
    ID = 2,
    POSITION = "Laboratory Assistant",
    DATES = "2012-01 - 2013-01",
    INSTITUTION = "The University of Texas at Austin: Texas Natural Science Center",
    LOCATION = "Austin, TX, USA",
    DESCRIPTION = c(
      "Catalogued, conserved, photographed, and archived acadmic fish and fossils collections",
      "Maintained and updated relational database management system for institution collections",
      "Trained volunteers and students in laboratory safety and inventory procedures"
    ),
    OUTPUT = "cv"
  ) |>
  tibble::add_row(
    ID = 1,
    POSITION = "Laboratory Assistant",
    DATES = "2011-03 - 2012-12",
    INSTITUTION = "Texas State University: Department of Geography",
    LOCATION = "San Marcos, TX, USA",
    DESCRIPTION = c(
      "Established geology and palaeontology laboratories for the university",
      "Prepared vertebrate fossils for laboratory use and public display",
      "Trained volunteers in laboratory procedures and sample preparation techniques",
      "Organised and led field exercises for sample collections and cleanings",
      "Captured and processed ichnological and palaeontological data using Structure from Motion",
      "Curated departmental fossils and rock collections"
    ),
    OUTPUT = "cv"
  )
## -----------------------------------------------------------------------------
### TEACHING EXPERIENCE ----------------------------------------------------
TEACHING_EXPERIENCE <-
  tibble::tribble(
    ~ ID,
    ~ POSITION,
    ~ DATES,
    ~ INSTITUTION,
    ~ LOCATION,
    ~ DESCRIPTION
  ) |>
  tibble::add_row(
    ID = 3,
    POSITION = "Data Analyst",
    DATES = "2021-02 - present",
    INSTITUTION = "University of Tasmania",
    LOCATION = "Sandy Bay, TAS, Australia",
    DESCRIPTION = c("Designing and implementing programming and data analytics training seminars for institution staff",
                    "Overseeing and contributing weekly learning and showcasing workshops")
  ) |>
  tibble::add_row(
    ID = 2,
    POSITION = "Substitute Teacher",
    DATES = "2017-01 - 2017-08",
    INSTITUTION = "Kelso School District: Beacon Hill Elementary School",
    LOCATION = "Kelso, WA, USA",
    DESCRIPTION = c(
      "Taught a variety of classrooms, with a focus on K-5 primary education, special education, and LAP classrooms using specialised learning plans",
      "Engaged young students with an interest in scientific research, giving special outreach presentations",
      "Helped integrate and refine special education lesson plans as an instructional assistant"
    )
  ) |>
  tibble::add_row(
    ID = 1,
    POSITION = "Teaching Assistant",
    DATES = "2013-08 - 2016-06",
    INSTITUTION = "Western Washington University: The Department of Environmental Sciences",
    LOCATION = "Bellingham, WA, USA",
    DESCRIPTION = c(
      "Coordinated and taught laboratory classrooms for analytical GIS, remote sensing, and research methodology",
      "Led overnight field exercises in inclement weather and rough terrain",
      "Coordinated international travel and classrooms for students and participating agencies",
      "Created and maintained GIS geodatabases and associated topologies",
      "Edited and performed technical reviews of student research papers at the developmental, substantive, and line level",
      "Classes taught: Remote Sensing (ESCI 442), Introduction to GIS (ENVS 320), Computer Cartography (ENVS 321), Research and Writing (ENVS 319), Environmental Data and Information (ENVS 201), Environmental History and Ethics (ENVS 305), Physical Geography (ENVS 203), the Soil Environment and Landscapes (ENVS 427), Water Resources (ENVS 427), Agroecology and Sustainable Agriculture (ENVS 410), Food Cultures of Italy (ENVS 497), and Ecogastronomy (ENVS 110)"
    )
  )

CONSULTING <-
  tibble::tribble(
    ~ DATE,
    ~ AGENCY,
    ~ PROJECT,
    ~ AMOUNT
  ) |>
  tibble::add_row(
    DATE = "2018-08",
    AGENCY = "U.S. Forest Service",
    PROJECT = "Restoration treatment effects on ponderosa pine growth sensitivity to climate",
    AMOUNT = "USD\\$2,750"
  ) |>
  tibble::add_row(
    DATE = "2019-06",
    AGENCY = "U.S. Forest Service",
    PROJECT = "Post-fire logging effects on reburn severity",
    AMOUNT = "USD\\$2,750"
  ) |>
  tibble::add_row(
    DATE = "2020-08",
    AGENCY = "Frontier Development Lab",
    PROJECT = "Predicting fire behaviour using historical satellite imagery",
    AMOUNT = "AUD\\$1,000"
  )


HONOURS <-
  tibble::tribble(
    ~ DATE,
    ~ AGENCY,
    ~ HONOUR,
    ~ AMOUNT
  ) |>
  tibble::add_row(
    DATE = "2021-10",
    AGENCY = "Australian Museum",
    HONOUR = "NSW Environment, Energy and Science (DPIE) Eureka Prize for Applied Environmental Research",
    AMOUNT = "AUD\\$10,000"
  ) |>
  tibble::add_row(
    DATE = "2021-09",
    AGENCY = "University of Tasmania",
    HONOUR = "Sustainability Award",
    AMOUNT = NA_character_
  ) |>
  tibble::add_row(
    DATE = "2021-08",
    AGENCY = "Ten Lives Cat Centre",
    HONOUR = "Certifi-Cat of Appreciation",
    AMOUNT = NA_character_
  ) |>
  tibble::add_row(
    DATE = "2020-08",
    AGENCY = "Frontier Development Lab",
    HONOUR = "Bushfire Data Quest 2020",
    AMOUNT = "AUD\\$1,000"
  ) |>
  tibble::add_row(
    DATE = "2019-09",
    AGENCY = "NSW Bushfire Risk Management Research Hub",
    HONOUR = "Tasmania Graduate Research Scholarship",
    AMOUNT = "AUD\\$98,000"
  ) |>
  tibble::add_row(
    DATE = "2019-09",
    AGENCY = "NSW Bushfire Risk Management Research Hub",
    HONOUR = "NSW Bushfire Risk Management Research Hub Top-Up",
    AMOUNT = "AUD\\$17,500"
  ) |>
  tibble::add_row(
    DATE = "2018-06",
    AGENCY = "Washington Department of Fish and Wildlife",
    HONOUR = "Best Fish and Wildlife Science",
    AMOUNT = NA_character_
  ) |>
  tibble::add_row(
    DATE = "2017-10",
    AGENCY = "Washington Department of Fish and Wildlife",
    HONOUR = "Conservation Award",
    AMOUNT = NA_character_
  ) |>
  tibble::add_row(
    DATE = "2016-04",
    AGENCY = "Western Washington University",
    HONOUR = "Dean and Sandy Blinn Travel and Research Fund",
    AMOUNT = "USD\\$450"
  ) |>
  tibble::add_row(
    DATE = "2016-04",
    AGENCY = "Western Washington University",
    HONOUR = "Dean's Fund for Sustainability Studies",
    AMOUNT = "USD\\$500"
  ) |>
  tibble::add_row(
    DATE = "2014-07",
    AGENCY = "North American Dendroecological Fieldweek",
    HONOUR = "North American Dendroecological Fieldweek research fellowship",
    AMOUNT = "USD\\$3,000"
  ) |>
  tibble::add_row(
    DATE = "2014-05",
    AGENCY = "Association of Washington Geographers",
    HONOUR = "Award for Outstanding Poster Presentation by a Graduate Student",
    AMOUNT = "USD\\$125"
  )

