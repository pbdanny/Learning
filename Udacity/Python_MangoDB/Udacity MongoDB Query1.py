from pymongo import MongoClient
client = MongoClient('mongodb://localhost:27017')
db = client.example
    
def range_query():
    query = {"dimensions.weight" : {"$gt" : 500}}
    return query

query = range_query()
autos = db.autos.find(query)

print("Found autos:", autos.count())

import pprint

for i in autos:
    pprint.pprint(i)