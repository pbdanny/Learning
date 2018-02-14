# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""
import os
import codecs
import csv
import json
import pprint

os.chdir('/Users/Danny/Documents/Python Project/UdactiyMangoDB')

CITIES = 'cities.csv'

FIELDS = ["name", "timeZone_label", "utcOffset", "homepage", "governmentType_label",
          "isPartOf_label", "areaCode", "populationTotal", "elevation",
          "maximumElevation", "minimumElevation", "populationDensity",
          "wgs84_pos#lat", "wgs84_pos#long", "areaLand", "areaMetro", "areaUrban"]

header = []
data = []


with open(CITIES, 'r') as file:
    reader = csv.reader(file)
    
    # save header
    header = next(reader, None)
    
    # skip first 3 lines
    for i in range(3):
        next(reader, None)
    # read in data row by row
    for line in reader:
        data.append(line)
# Helper function check if string could be casted to int
def string_to_int(s):
    try:
        int(s)
        return True
    except ValueError:
        return False
# Helper function check if string could be casted to float
def string_to_float(s):
    try:
        float(s)
        return True
    except ValueError:
        return False

# Output varibles
fieldtypes = {}
for col in range(len(header)):            
    t = set()
    # use list comprehension to travorse 
    dataCol = [item[col] for item in data]
    for i in dataCol:
        if i == 'NULL' or i == "": # None Type
            t.add(type(None))
        elif i[0] == '{':  # List type start with {
            t.add(type([]))
        elif string_to_int(i):
            t.add(type(1))
        elif string_to_float(i):
            t.add(type(1.1))
        else:
            t.add(type('a'))
            
    fieldtypes[header[col]] = t