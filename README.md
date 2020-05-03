# dromedary

#### Load testing framework in Haskell

The purpose of this library is to provide a load testing framework consiting of two basic features:
1. Take an IO action and run it repeatedly, registering success/failure.
2. After a given interval (testing period) stop the IO actions, and print statistics on the number of successes/failures.


