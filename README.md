# timecache.hs

[![Build Status](https://magnum.travis-ci.com/ga2arch/timecache.hs.svg?token=fVfRqtJ2xpjs3BsujiSp&branch=master)](https://magnum.travis-ci.com/ga2arch/timecache.hs)

Simple cache where each element that expire gets sent to a webhook.

## Usage
```
timecache - simple cache with expiring events

Usage: timecache [--db NAME] [--port PORT] --hook URL [--interval TIME]
  Run the cache

Available options:
  -h,--help                Show this help text
  --db NAME                The name of the db file
  --port PORT              The port to listen on
  --hook URL               The url of the hook
  --interval TIME          The interval between two checks
```

## Insert element

Make a POST request to ```http://localhost:8080/insert``` with json document of form:

```json
{
  "key":       "test"
, "value":     "data"
, "timestamp": 1448198760
}
```

When 1448198760 is >= now, "data" is sent to the webhook.


## Update element
When an insertion is performed, if the key is already in the db it gets updated

## Delete element

Make a DELETE request to ```http://localhost:8080/delete/<key>```

## Get element

Make a GET request to ```http://localhost:8080/entries/<key>```
