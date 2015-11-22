# timecache.hs

[![Build Status](https://magnum.travis-ci.com/ga2arch/timecache.hs.svg?token=fVfRqtJ2xpjs3BsujiSp&branch=master)](https://magnum.travis-ci.com/ga2arch/timecache.hs)

Simple cache where each element that expire gets sent to a webhook.

## setWebhook

Make a post request to ```http://localhost:8080/setWebhook?hook=<url>```

## Insert element 

Make a post request to ```http://localhost:8080/``` with json document of form:

```json
{
  "value":     "data"
, "timestamp": 1448198760
}
```

When 1448198760 is >= now, "data" is sent back to the url passed in the setWebhook

