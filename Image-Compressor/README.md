# Image-Compressor
A program - implemented in C - that defines an image file format “P152” that encodes images with either run-length encoding and/or an indexed color table, depending on user choice.

The different C files convert ppm files (P3 and P6) into P152 and back (e.g. ppm_p152.c converts P3 and P6 files to P152).

The file describe.c takes in a P152 image and writes the following metadata of the file to stdout:

- the date embedded in the file
- the time embedded in the file
- the string description embedded in the file
- the dimensions of the image
- whether of not the image uses run-length encoding
- whether or not the image is grayscale
- whether or not the image uses a color table, and, if, so, the size of the table

The file negative.c takes in a P3 or P6 image and returns the negative of the image. For a given RGB-pixel, with values of 0-255 for each color coordinate, the negative of pixel (r,g,b) is (255 - r, 255 - g, 255 - b).

The ppm_p152 tool is able to read both P3 and P6 files, and supports the following command-line options:

-i [infile] read from the named file instead of stdin. Reads from stdin if absent.

-o [outfile] write to the named file instead of stdout. Writes to stdout if absent.

-t [n] construct a color-table of given size n (0<n<=256). No color table if absent.

-r use run-length encoding. Does not run-length encode if absent.






