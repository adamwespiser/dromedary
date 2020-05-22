# dromedary

#### Load testing framework in Haskell

The purpose of this library is to provide a load testing framework consiting of two basic features:
1. Take an IO action and run it repeatedly, registering success/failure.
2. After a given interval (testing period) stop the IO actions, and print statistics on the number of successes/failures.


#### Run Example/Test
Start the test API
```
$ stack run camel-server
```
Then, in a new window run the actually load test:
```
$ stack run dromedary
```
This will produce the following results:

```
Results
Experiment 1
    Pass: 4615
    Fail: 0
Experiment 2
    Pass: 2335
    Fail: 2350
```



