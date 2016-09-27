# bigpanda
BigPanda Exercise

Your exercise is to implement a simple stream processing service that also exposes an HTTP interface.

You are provided with a blackbox executable that spits out an infinite stream of lines of event data encoded in JSON. You can download it here:
* Linux - https://s3-us-west-1.amazonaws.com/bp-interview-artifacts/generator-linux-amd64
* Mac OS X - https://s3-us-west-1.amazonaws.com/bp-interview-artifacts/generator-macosx-amd64
* Windows - https://s3-us-west-1.amazonaws.com/bp-interview-artifacts/generator-windows-amd64.exe

Please note that the stream might sometimes encounter errors and output corrupt JSON lines.

These are the requirements for your service:
- it should consume the output of the generator and gather the following stats: a count of events by event type and a count of words encountered in the data field of the events.
- it should expose these stats in an HTTP interface
- the processing of the generator data and the handling of the HTTP requests should not block each other

The architecture of your service should obviously decouple the data processing, HTTP handling, be testable, etc. 

## Quick start

Install [stack](https://docs.haskellstack.org/en/stable/README/)

Build and execute
```
stack build

stack exec bigpanda-exe
```

Query the server
```
curl 0:8000
```
