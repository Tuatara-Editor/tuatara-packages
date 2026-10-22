# csvdt

[csvdt](https://github.com/YardQuit/csvdt) run over a CSV buffer, a selection
or banked lines, the result in a buffer of its own, `*csvdt output*`. csvdt
converts the timestamps and durations of CSV records; this package hands it
whole records from the buffer, in the background, and never changes the
buffer they came from.

csvdt is a program of its own, installed apart from Tuatara.

## Loading it

In `~/.config/tuatara/init.lisp`, with this repository named as a source:

```lisp
(load-package "csvdt" :source "tuatara-packages")
```

If `csvdt` is not on the PATH, set `*csvdt-program*` after `:config`. A name
with a slash in it means that file and is never looked for on the PATH; a
relative one, `"./target/release/csvdt"`, is taken from the directory of the
buffer you run it in.

```lisp
(load-package "csvdt" :source "tuatara-packages"
  :config
  (setf *csvdt-program* "~/src/csvdt/target/release/csvdt"))
```

The settings, each with its help in `C-h v`:

| Setting | Default |
|---|---|
| `*csvdt-program*` | `"csvdt"` |
| `*csvdt-default-arguments*` | `"-H -p"` |
| `*csvdt-region-carries-header*` | `t` |
| `*csvdt-header-options*` | `("-H" "--has-header")` |
| `*csvdt-single-quote-options*` | `("--single-quote")` |
| `*csvdt-read-delimiter-options*` | `("--read-delimiter")` |
| `*csvdt-output-mode*` | `csv-mode` |

## Commands

In CSV and TSV buffers, under `C-c C-v`:

| Key | Command | What it does |
|---|---|---|
| `C-c C-v r` | `csvdt-run-dwim` | the banked lines and the selection together, else the whole buffer |
| `C-c C-v b` | `csvdt-run-buffer` | the whole buffer |
| `C-c C-v SPC` | `csvdt-run-region` | the selection, to try something on a few rows first |
| `C-c C-v k` | `csvdt-run-banked` | the banked lines |
| `C-c C-v ?` | `csvdt-describe-run` | what a run would cover, without running it |
| `C-c C-v h` | `csvdt-help` | csvdt's own `--help` |
| `C-c C-v v` | `csvdt-version` | which csvdt, and which time zone database is in it |

Anywhere else they are run with `M-x`, or a key of your own:

```lisp
(bind-key "; c" 'csvdt-run-dwim :map :leader)   ; SPC ; c
```

## Arguments

Typed in the minibuffer as a shell line, prefilled from
`*csvdt-default-arguments*` and **left selected**:

| Key | What it does |
|---|---|
| type anything | replaces the prefill |
| `→` or `C-f` | keeps it, to add to |
| `RET` | runs it as it stands |
| `TAB` | completes the option before the cursor, from what csvdt reports |
| `M-p` / `M-n` | back and forth through lines run before |

The prefill is selected rather than only offered because leaving `-H` off a
file with a header is loud, while passing it to a file without one is quiet:
the first row is taken for a header and goes through unconverted. The prompt
names the csvdt that will run.

The line is split as a shell splits it, not on commas, since a comma is
csvdt's own separator inside a value (`-a 1,0`, `-i 0,replace`). Either quote
works. Double quotes take `\t` `\n` `\r` `\\` `\"` and refuse any other
backslash, so `"C:\data"` is refused rather than misread; single quotes take
everything as it is. Outside quotes a backslash is itself. Nothing is run
through a shell.

A filename in the line is refused: these commands give csvdt the lines on its
standard input, and a file named would take their place in a result that
looks exactly like a run over them. Only a name that is a file on disk is
refused, and which arguments are values rather than filenames comes from
`csvdt --list-options`, so `-u 0` with a file called `0` lying about is still
a run.

## Nothing here needs updating when csvdt changes

The help is csvdt's own `--help`, read from the program each time. The
options offered for completion come from `csvdt --list-options`, kept against
the program's real path and the time it was written, so a rebuilt csvdt is
asked again and an unchanged one is not run just to find that out. A csvdt
too old to list them has them read from `--help` instead.

Only three options are named, each in a setting, because a run over part of a
buffer cannot be worked out without them: the header option, the single
quote and the read delimiter. The tests hold each to an option csvdt still
reports, and fail if any other option csvdt reports is named in the package.

## What gets processed

`csvdt-run-dwim` takes the banked lines **and** the selection, merged so a
line in both goes once; with neither, the whole buffer.

Banked lines are Tuatara's own (`bank-selected-lines`, `m l` in NORMAL
state). They go to csvdt as one file in buffer order, so a duration is
measured across what was banked rather than across what lies between, and
running over them leaves them banked.

The selection is taken as `y` and `d` take it: a line selection whole, a
rectangle by the rows it spans. A rectangle's columns are ignored: picking
columns is csvdt's job (`--remove`, `-a`). A run takes the main selection
only, not the places of cursors or a split.

Whatever was picked, whole records are what go out:

- A selection starting or ending inside a line takes the whole line, and the
  run says `, selection widened to whole lines`.
- A quoted field can hold a line end, so a record is not always a line. The
  range is pushed out to record boundaries, and the run says `, widened to
  whole records: a quoted field spans lines`. Finding the boundary reads from
  the start of the buffer, quote to quote; a whole-buffer run reads nothing.
- Two banked lines inside one multi-line record widen to the same record, and
  it is sent once.
- When the arguments ask for a header and what is sent starts below it, the
  buffer's header record goes in front, so csvdt does not take your first
  selected row for the header. Set `*csvdt-region-carries-header*` to nil to
  stop it.

`--single-quote` and `--read-delimiter` are read from the arguments before
any of that, since they decide where a record begins. `csvdt-describe-run`
reads them from `*csvdt-default-arguments*`, having no line to read yet.

The buffer's bytes go to csvdt as they are, so a file that is not UTF-8
reaches csvdt as it is on disk and csvdt refuses it in its own words. Line
ends are sent as `\n` when the buffer's are `\r\n`.

## Running, and the output

A run is a job. Quick ones look instant; one that takes more than half a
second carries on in the background with the editor free, `P` lit in the
mode line, listed in `C-x j`, and `C-x J` (`M-x stop-job`) ends csvdt. A
second run over the same buffer while one is going is refused rather than
queued.

The output goes to `*csvdt output*` in `*csvdt-output-mode*` (csv-mode),
which falls back to fundamental-mode if the mode cannot be entered rather
than losing the result. The buffer stays one to edit: sort its lines, run
csvdt on it again to feed a result back through. Lines banked in the previous
result are let go, the text they stood for being gone.

The source buffer is never changed. On success the echo area gets the line
count, the stretches when there was more than one, the widening notes above,
and anything csvdt said on stderr about a run that still succeeded:

```
csvdt -H -p -u0 -i 0,replace: 3 lines from 2 stretches
```

On failure it gets csvdt's own reason and exit status, and what csvdt wrote
before failing is shown in `*csvdt output*`, except when the run was fed from
that buffer, whose text is kept:

```
csvdt exited 100: '--utc' was given "0-i", which is not a column number. Columns are named by position, counted from 0.
```

## Tests

`tools/test.sh` runs them with the other packages'. The runs of csvdt need
one: `CSVDT=/path/to/csvdt`, else the one on the PATH; without one they are
skipped, not passed.
