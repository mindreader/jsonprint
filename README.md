# jsonprint
Allows a pretty printing of a json value at every level of its heirarchy so that you can inspect it easily via commandline.

Imagine a file "blah.json" had the string

```json
[1,2,3,{"asdf": 10000000.1, "anarray": [100], "fdsa": 1.0, "blah":1},0.001, null, [1,2,3]]
```

```bash
> $ jsonprint blah.json # or cat blah.json | jsonprint
[0]: 1
[1]: 2
[2]: 3
[3,"blah"]: 1
[3,"fdsa"]: 1
[3,"asdf"]: 10000000.1
[3,"anarray",0]: 100
[3,"anarray"]: [100]
[3]: {"anarray":[100],"blah":1,"fdsa":1,"asdf":10000000.1}
[4]: 0.001
[5]: null
[6,0]: 1
[6,1]: 2
[6,2]: 3
[6]: [1,2,3]
[]: [1,2,3,{"anarray":[100],"blah":1,"fdsa":1,"asdf":10000000.1},0.001,null,[1,2,3]]
```

Which allows you to easily grep for parts of the file you want.

```bash
> jsonprint ~/bit/out2.txt  | grep '^"results",4:'
"results",4: 0.001
$ jsonprint ~/bit/out2.txt  | grep '^"results",3,"anarray":'
"results",3,"anarray": [100]
```
