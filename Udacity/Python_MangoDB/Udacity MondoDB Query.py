#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Apr 14 13:42:20 2017

@author: Danny
"""

import os

os.chdir("/Users/Danny/Documents/Python Project/UdactiyMangoDB")

from pymongo import MongoClient
client = MongoClient('mongodb://localhost:27017')
db = client.example

from datetime import datetime
    
def range_query():
    # Modify the below line with your query.
    # You can use datetime(year, month, day) to specify date in the query
    query = {}
    return query

query = range_query()
cities = db.cities.find(query)

print "Found cities:", cities.count()
import pprint
pprint.pprint(cities[0])