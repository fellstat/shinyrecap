# Upload Data

Select a text file on your local machine. This file can have records that are separated by commas (csv format), semicolons, tabs or spaces.

--------------------

The analysis application works with data in "Capture History" format. Each column should represent a capture event and each row should be a capture history. Optionally the last column may be the count of individuals with that particular capture history. "1" indicates capture, and "0" indicates not captured, so the history
```
0 1 0 1
```
represents an individual who was captured in the 2nd and 4th event, but not the 1st or 3rd. A properly structured 3 event CRC dataset would look something like:
```
V1  V2  V3  count
1   0   0   3
0   1   0   8
1   1   0   4
0   0   1   6
1   0   1   2
0   1   1   2
1   1   1   1
```
From the first row `1	  0	  0	  3`, wee see that there were 3 individuals captured at event 1, but not at the 2nd and 3rd events. There was 1 individual captured in all 3 events (row: `1	  1	  1	  1`).

--------------------

See the [Manual](https://fellstat.github.io/shinyrecap/) for more information.

