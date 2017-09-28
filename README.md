hUninvited
=========
NES Uninvited text tool. Game uses Golomb coding for text. This tool can work with it.


Synopsis:
```
huninvited [-d | -c | -k] fileName
```
  
Description

***hUninivted -d*** <file>  Decompress script from given ROM file.

***hUninivted -c*** <file>  Compress script and insert in given ROM file.

***hUninivted -k*** <file>  Check given script file for excess length.

***-h, --help*** - Display help

***-v, --version*** - Output version information

See additional files in [release](https://github.com/romhack/hUninvited/releases/latest) archive. Recommended translation scheme:  

1. run decompress.bat

2. prepare encode.tbl and modify ROM graphics as you need
  
3. modify script text files and run check.bat for your convenience

4. run compress.bat, check the result in emulator, modify text and press any key in cmd window to reinsert it.
  

Tables are in Nightcrawler's table file standard. In short, $12=[item],lo,hi -control code with parameters, and /E0=[end]\n\n -end code  
Script is autowrapped on 27th symbol. So original game just don't break line, if current line is broken on exactly 27th symbol.  
But to eliminate your mistakes, I recommend running check.bat and recheck each line.  

Build with [Haskell Stack](https://haskellstack.org) tool.