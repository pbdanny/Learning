#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Apr 16 23:11:06 2017

@author: Danny

Analyzing data with MongoDB aggregration
"""
import pprint

from pymongo import MongoClient
client = MongoClient('mongodb://localhost:27017')
db = client.twitter

# Group & Sort
def make_pipeline1():
    pipeline = [
            {'$group': {'_id': '$source', 'count': {'$sum': 1}}},
            {'$sort': {'count': -1}}
            ]
    return pipeline

# Match & project & sort
def make_pipeline2():
    pipeline = [
                {'$match': {'user.time_zone': 'Brasilia'}},
                {'$match': {'user.statuses_count': {'$gt': 100}}},
                {'$project': {'_id': 1, 
                                'followers': '$user.followers_count', 
                                'screen_name': '$user.screen_name', 
                                'tweets': '$user.statuses_count'}},
                {'$sort': {'followers': -1}},
                {'$limit': 1}
            ]
    return pipeline

# Unwind array -> copy of document with spread each items in array  
def make_pipeline3():
    pipeline = [
            {'$match': {'country' : 'India'}},
            {'$unwind': '$isPartOf'},
            {'$group': {'_id': '$isPartOf',
                        'count': {'$sum': 1}}},
            {'$sort': {'count': -1}},
            {'$limit': 1}
        ]
    return pipeline

# Push create array of aggregate data
def make_pipeline4():
    pipeline = [
                {'$group': {'_id': '$user.screen_name',
                            'count': {'$sum': 1},
                            'tweet_texts': {'$push': '$text'}}},
                {'$sort': {'count': -1}},
                {'$limit': 5}
    ]
    return pipeline

# Repate operator in aggregate pipeline
def make_pipeline5():
    pipeline = [
                {'$match': {'country': 'India'}},
                {'$unwind': '$isPartOf'},
                {'$group': {'_id': '$isPartOf',
                            'avgCityPop': {'$avg': '$population'}}},
                {'$group': {'_id': 'Avg Pop of all Region',
                            'avg': {'$avg': '$avgCityPop'}}}
    ]
    return pipeline


def tweet_sources(db, pipeline):
    return [doc for doc in db.tweets.aggregate(pipeline)]

pipeline = make_pipeline1()
result = tweet_sources(db, pipeline)
pprint.pprint(result[0])

pipeline = make_pipeline2()
result = tweet_sources(db, pipeline)
pprint.pprint(result[0])