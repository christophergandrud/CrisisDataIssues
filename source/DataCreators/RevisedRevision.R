#############
# Script to create Revision variable that treats start year changes as revisions
# Christopher Gandrud
# 25 February 2014
#############

## Rules
# Merge older version into the newer year record

## Oddities
# Australia 1989 is included in Honohan and Klingebiel (2003), but not others
# Egypt 1996 is included in Honohan and Klingebiel (2003), but not others
# Indonesia 1992 is included in Honohan and Klingebiel (2003), but not others
# Indonesia 1994 is included in Caprio and Klingebiel (2003), but not others
# Malaysia 1985 missing from Laeven and Valencia (2012)


CombRevis$YearMergeRevise <- CombRevis$Revision

CombRevis$YearMergeRevise[CombRevis$iso2c == 'EC' & CombRevis$year == 1996] <- NA
CombRevis$YearMergeRevise[CombRevis$iso2c == 'EC' & CombRevis$year == 1998] <- 1

CombRevis$YearMergeRevise[CombRevis$iso2c == 'JP' & CombRevis$year == 1991] <- NA
CombRevis$YearMergeRevise[CombRevis$iso2c == 'JP' & CombRevis$year == 1992] <- NA
CombRevis$YearMergeRevise[CombRevis$iso2c == 'JP' & CombRevis$year == 1997] <- 1

CombRevis$YearMergeRevise[CombRevis$iso2c == 'MX' & CombRevis$year == 1995] <- NA
CombRevis$YearMergeRevise[CombRevis$iso2c == 'MX' & CombRevis$year == 1994] <- 1

CombRevis$YearMergeRevise[CombRevis$iso2c == 'NO' & CombRevis$year == 1987] <- NA
CombRevis$YearMergeRevise[CombRevis$iso2c == 'NO' & CombRevis$year == 1991] <- 1

CombRevis$YearMergeRevise[CombRevis$iso2c == 'PH' & CombRevis$year == 1998] <- NA
CombRevis$YearMergeRevise[CombRevis$iso2c == 'PH' & CombRevis$year == 1997] <- 1

CombRevis$YearMergeRevise[CombRevis$iso2c == 'TR' & CombRevis$year == 1994] <- 1 # only in Honohan & Klingebiel (2003)

CombRevis$YearMergeRevise[CombRevis$iso2c == 'US' & CombRevis$year == 1981] <- NA
CombRevis$YearMergeRevise[CombRevis$iso2c == 'US' & CombRevis$year == 1984] <- NA
CombRevis$YearMergeRevise[CombRevis$iso2c == 'US' & CombRevis$year == 1988] <- 1
