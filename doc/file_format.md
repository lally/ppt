# File Formats

I'm punting early on the first-round file format for saved dagta.
 - 4 bytes Magic Number (0x50505431, `PPT1`)
 - 4 Byte header length (little endian), named `k`
 - `k` bytes of JSON
 - remainder of file is saved buffers.


