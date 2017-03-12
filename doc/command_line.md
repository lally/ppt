PPT Command Line Interface
==========================

# Basic Commands
`generate` -- Generate source code from an input .spec.
`record` -- Attach to a process and save data from its buffer.
`convert` -- Convert a recording to a readable form.
`query` -- List properties from a running process (or executable?)

## Generate
- Needs the spec file and parameters for filename generation.
```
ppt generate [--prefix <pfx>] [--tag <key> <value>] [--option <options...>] filename
```

- Also needs tags or other options.
- `--tag <key> <value>`
- `--option [option syntax]`
- They're all saved into the JSON.

## Record
- Needs a pid and an output file.
```
ppt record -p <pid> -b <buffer> -o <filename> [-s bufsize]
```

- V1 won't be a .tar file.  It requires spares file support (and a
  giant file alloc, potentially).  So, how do I save the file format
  along with the raw data?  Maybe something very simple: a dumb header,
  maybe something that's compatible with .tar (for `convert` syntax).

- The filename is just first.  So we'll just shove a non-ascii
  character first.  Maybe a single-byte 1, followed by the length of
  the json (4 bytes, little endian), then the JSON, then the data from
  the struct.

## Convert
- Needs a filename, format, and output name.
```
ppt convert -f <filename> -t <format> -o <result>
```
    Perhaps I can infer the result from the output filename?


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
