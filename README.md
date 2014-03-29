lwml-preview
=========
light weight markup language preivewer.

engine/format
--------
* pandoc(pandoc formats)
* github(markdown)

help
--------
```.sh
Usage: lwml-preview [-p|--port PORT] FILE [-c|--css FILE] ([--pandoc] |
                    [-g|--github]) [-i|--input TYPE] [-s|--style STYLE]

Available options:
  -h,--help                Show this help text
  -p,--port PORT           port number (default: 8000)
  -c,--css FILE            custom css file
  --pandoc                 use pandoc engine (default)
  -g,--github              use github engine
  -i,--input TYPE          input type of pandoc
  -s,--style STYLE         syntax highlight of pandoc
```
