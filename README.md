# watcher

A Haskell program that watches a list of URLs and notifies you on Discord & Twilio when something goes wrong. To be specific, when some server dies. I currently use this to watch multiple microservices to make sure nothing has crashed.

## Basic Usage 

Requirements;

1. Twilio account id, auth token, to number, from number
2. Discord channel ID
3. A set of urls such as `https://yourserver.com/api/v1/ok,https://yourotherserver.com/api/v1/ok`

```sh
cp .env-example .env # and then modify the values
cabal install && cabal run
```
