## Day 1

```
$ time node 01-02.js
73056
node 01-02  0,08s user 0,00s system 102% cpu 0,085 total

$ time stack runghc 01-02.hs
Just 73056
stack runghc 01-02.hs  156,07s user 0,10s system 100% cpu 2:36,02 total
```

After change `[(Int, Int)]` to `Set`

```
$ time stack runghc 01-02.hs
Just 73056
stack runghc 01-02.hs  0,90s user 0,04s system 103% cpu 0,912 total
```
