PPT Command Line Interface
==========================

# Basic Commands
`generate` -- Generate source code from an input .spec.
`attach` -- Attach to a process and save data from its buffer.
`decode` -- Convert a recording to a readable form.

## Generate
- Needs the spec file and parameters for filename generation.
```
ppt generate [options] <spec-file>
Options:
  --prefix FILE-PREFIX
  --tag KEY VALUE
  --option OPITON
```

- Also needs tags or other options.
- `--tag <key> <value>`
- `--option [option syntax]`
- They're all saved into the JSON.

## Attach
- Needs a pid and an output file.
```
ppt attach
  -p pid              --pid=pid                   Pid to attach to. (required)
  -b buffer name      --buffer=buffer name        Name of buffer to read.  (required)
  -l                  --list-buffers              List buffers in process.
  -h, -?              --help                      Print help.
  -d description      --desc=description          A comment on the collection session.
  -D descriptionfile  --descfile=descriptionfile  File containing comment on the collection session.
  -t delta-hours      --time-offset=delta-hours   Apply this change to the timestamps on this machine.
  -o output-file      --output-file=output-file   Output file to save buffer contents. (required)
  -c counter-name     --counter=counter-name      Comma-separated list of performance counters to use
  -v                  --verbose                   Verbose output.

```

- V1 won't be a .tar file.  It requires spares file support (and a
  giant file alloc, potentially).  So, how do I save the file format
  along with the raw data?  Maybe something very simple: a dumb header,
  maybe something that's compatible with .tar (for `convert` syntax).

- The filename is just first.  So we'll just shove a non-ascii
  character first.  Maybe a single-byte 1, followed by the length of
  the json (4 bytes, little endian), then the JSON, then the data from
  the struct.

## Decode
- Needs a filename.  Puts out a directory with one CSV file per buffer type.

```
ppt decode input_filename [output-dir]
  where output-dir will be generated if unpsecified.
```


## Query
- Needs a pid or executable, and then something to show.
```
ppt query [-p <pid> | -f <file>] [-l] [-d <buffer>] [-t <buffer> (or --all)]
```
- It's by buffer.  List buffers with `-l`.
- Per buffer, we can describe the buffer, or list tags.
- Tags are attached per buffer.

# Parser Setup

Each command has its own args parser.  In fact, each command should
have its own module.
