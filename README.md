hUninvited
=========
NES Uninvited text tool. Game uses Golomb coding for text. This tool can work with it.


Synopsis:
```
huninvited [-d | -s | -i | -e] arguments
```
  
Description

***-d, --decode*** input_file index_map_offset text_offset  - Decode one message from ROM at given offset

***-s, --script*** input_file index_map_offset pointer_table_offset pointer_count text_base_offset - Batch decode script from ROM by pointer table

***-i, --index*** input_file... - Build binary index table from given decoded binary files

***-e, --encode*** input_file index_table pointer_table_start_RAM_address - Encode decoded binary file with given binary index table file

***-o[FILE], --output[=FILE]*** - Output to binary FILE; if option is not specified, -d and -s are output to stdout

***-h, --help*** - Display help

***-v, --version*** - Output version information

Usage examples are in run.bat file. Recommended translation scheme:  
  
1: run.bat decode  
  
2: modify decoded text groups  
  
3: run.bat encode
  
4: run.bat paste
  
5: go to 2