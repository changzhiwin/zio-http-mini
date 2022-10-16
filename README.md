# About

This is a self traing project, deeply understand [zio-http](https://github.com/zio/zio-http) souce code. **Not for entry level**.

# How to run
- scala 2.13.8
```
> sbt
> project example
> run
```

## Test
[httpie](https://httpie.io/) is a very nice tool.
```
http get http://localhost:8080/abc/say/
```

## Output

```
sbt:example> run
[info] running (fork) example.RouteSyntax 
[info] > GET | /abc/say/ | Http_1_1
[info] > Host: localhost:8080
[info] > Accept-Encoding: gzip, deflate
[info] > Accept: */*
[info] > Connection: keep-alive
[info] > User-Agent: HTTPie/3.2.1
[info] > content-length: 0
[info] < Ok
[info] < content-type: application/json
[info] < {"star": "5", "full": "say"}
```
