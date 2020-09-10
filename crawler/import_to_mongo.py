#!/usr/bin/env python3

# takes a JSON file of onjects and a target collection to upload to
# TODO - DB target is hardcoded, make an arg
# TODO - assumes a "number" field to construct the ID, won't work for other crawls
# TODO - add proper environment-based auth in MongoClient call
#
# Example:
# ./import_to_mongo.py --collection issues ./issues.json

import argparse
import json
import sys
import pymongo

parser = argparse.ArgumentParser()
parser.add_argument('--collection')
parser.add_argument('file', type=argparse.FileType('r'), default=sys.stdin,
                            nargs='?')
args = parser.parse_args()

myclient = pymongo.MongoClient("mongodb://localhost:27017/")
mydb = myclient["ansible_collections"]
mycol = mydb[args.collection]

items = json.load(args.file)

for item in items:
    id = item['repository']['nameWithOwner'] + '/' + str(item['number'])
    item['_id'] = id
    mycol.replace_one({'_id':id}, item, upsert = True)
