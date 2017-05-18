# knitPatterns
Haskell library for writing and combining knitting patterns.

So far focuses on cables and inclused m1l and m1r. Hopefully you can define your patterns as modifications over previous rows.

There are 'after' functions to produce the correct rows after a cable or make. 

There is a nextRow function that will allow you to take a list of patterns and take the first row of each to return the current work of progress. It also returns the patterns modified so the first row in each pattern is the one after and the returned row is now the last one in the pattern. This way it can easily combine patterns with different number of rows. 
