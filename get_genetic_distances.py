

from urielplus import urielplus
import pandas as pd

#initiate uriel
u = urielplus.URIELPlus()

#get iso_codes
iso_codes = pd.read_csv("raw_data/languages_iso_codes.csv")
iso = list(iso_codes.ISO)

#get distances
eu_distances = u.new_distance("genetic", iso)

#export distances
eu_distances = pd.DataFrame(eu_distances)
eu_distances.columns = list(iso_codes.language)
eu_distances.index =  list(iso_codes.language)
eu_distances.to_csv("clean_data/genetic_distances.csv")