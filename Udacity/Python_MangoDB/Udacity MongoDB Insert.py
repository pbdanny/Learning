#!/usr/bin/env python
"""
# shell command start MongoDB demon
# !mongod --dbpath /Users/Danny/data/db
"""
import os

os.chdir("/Users/Danny/Documents/Python Project/UdactiyMangoDB")
from autos import process_file

from pymongo import MongoClient
client = MongoClient('mongodb://localhost:27017')
db = client.example


def insert_autos(infile, db):
    data = process_file(infile)
    # Use insert many to insert list of dict into MongoDB
    db.autos.insert_many(data)
    
insert_autos('autos-small.csv', db)
print(db.autos.find_one())

client.close()