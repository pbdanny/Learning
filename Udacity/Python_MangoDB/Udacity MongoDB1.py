#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Apr 13 15:58:22 2017

@author: Danny
"""
# shell command start MongoDB demon
!mongod --dbpath /Users/Danny/data/db

from pymongo import MongoClient
client = MongoClient('localhost:27017')
db = client[db_name]
    
def porsche_query():
    query = {"manufacturer" : "Porsche"}
    return query

def find_porsche(db, query):
    return db.autos.find(query)


db.autos.insert_one({
	"layout" : "rear mid-engine rear-wheel-drive layout",
	"name" : "Porsche Boxster",
	"productionYears" : [ ],
	"modelYears" : [ ],
	"bodyStyle" : "roadster",
	"assembly" : [
		"Finland",
		"Germany",
		"Stuttgart",
		"Uusikaupunki"
	],
	"class" : "sports car",
	"manufacturer" : "Porsche"
})

query = porsche_query()

results = find_porsche(db, query)

print "Printing first 3 results\n"
import pprint
   for car in results[:3]:
       pprint.pprint(car)