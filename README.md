# graf

Plot discretized line charts in your terminal.


## Usage

```
graf [OPTION]...

Options:
    -h, --help           Print this help message and exit the program.
    -f, --file FILE      If FILE is - or not specified, read from stdin.
    -t, --title TITLE    Display TITLE on top of the plotted chart.
    -c, --color COLOR    Color to plot the line in. See options below.
    -s, --stats          Show statistics, which are hidden by default.
    -r, --range MIN:MAX  Fix plot bounds instead of choosing them dynamically.
    -d, --digits DIGITS  Ensure at least DIGITS significant digits are printed.
    -W, --width WIDTH    Maximum TUI width. Defaults to terminal width.
    -H, --height HEIGHT  Maximum TUI height. Defaults to terminal height.
```


## Examples

```sh
$ ping example.com \
| sed -u 's/.*time=\(.*\) ms/\1/' \
| graf -t "ping (ms)" --stats -W 80 -H 24
```

```sh
$ top -b -d 1 | grep --line-buffered 'Cpu(s)' \
| sed -u 's/.* \([0-9]\+\),[^ ]* id.*/\1/' \
| xargs -n 1 bash -c 'echo $((100 - $0))' \
| graf -t "CPU usage (%)" --range 0:100 -f - --color red
```

```sh
$ while true; do curl -sS -L -w '\n' http://api.coincap.io/v2/rates/bitcoin; sleep 1; done \
| sed -u 's/.*"rateUsd":"\([^"]*\)".*/\1/' \
| xargs -n 1 python3 -c 'import sys; print(float(sys.argv[1]) * 1e-8)' \
| graf -t "Satoshi price (USD per ₿ * 1e-8)" --digits 15 -s -c g
```
