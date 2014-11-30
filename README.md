hUninvited
=========
NES Uninvited text decompressor. Game uses variable bitwidth encoding scheme for text. This tool can decode it.


Usage:
```
hUninvited [-d | -b] file_name <offset> | <file_name ptr_offset ptr_count base_offset>
```


***-d*** - Decode one message from ROM at given offset. Input ROM name and offset in it must be specified: -d file_name offset

***-b*** - Batch decode script by pointer table. Input ROM, pointer table location, pointers count and base offset for text  must be specified: -b file_name ptr_offset ptr_count base_offset

Usage examples are in .bat file.