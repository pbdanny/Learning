#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Apr 13 15:58:22 2017

@author: Danny
"""
# shell command start MongoDB demon
!mongod --dbpath /Users/Danny/data/db

from pymongo import MongoClient

# Instantiate MongoDB data connector
client = MongoClient('localhost:27017')

# Use database examples; if database not exist then create one
db = client.examples

# from database 'example' use collection (table) 'cities'
# If the collection not exist then create one
def add_city(db):
    db.cities.insert_one({"name" : "Chicago"})


def get_city(db):
    return db.cities.find_one()

add_city(db)
print(get_city(db))