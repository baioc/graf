# graf

Plot line charts in your terminal.


## Examples

```sh
$ ping example.com \
| sed -u 's/.*time=\(.*\) ms/\1/' \
| graf
```

```sh
$ top -b -d 1 | grep --line-buffered 'Cpu(s)' \
| sed -u 's/.* \([0-9]\+\),[^ ]* id.*/\1/' \
| xargs -n 1 bash -c 'echo $((100 - $0))' \
| graf
```

```sh
$ while true; do curl -sS -L -w '\n' http://api.coincap.io/v2/rates/bitcoin; sleep 1; done \
| sed -u 's/.*"rateUsd":"\([^"]*\)".*/\1/' \
| xargs -n 1 python3 -c 'import sys; print(float(sys.argv[1]) / 1e8)' \
| graf
```
