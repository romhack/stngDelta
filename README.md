# stngDelta. 
Delta coding compression tool for 'Star Trek - The Next Generation' game.


Synopsis:
```
stngDelta [-d | -c] inFileName outFileName
```
  
Description:
```

stngDelta -d <inFile> <offset> <outFile> - Decompress block from given ROM file.

stngDelta -c <inFile> <outFile> - Compress given plain block.

-h - Display help

-v - Output version information
```

See additional files in [release](https://github.com/romhack/stngDelta/releases/latest) archive. 

Compression scheme is used in this game for tiles. It uses multiple modes of how to treat previous scanline of tile.  
This branch of tool code uses enabled mode 6 command for additional compression ratio. In the original ROM compression command 6X is not used and generally, has bugs in assembly code. To enable properly functioning mode 6 command, just paste these 16 bytes at ROM offset:  
```
0xC1F3: 2B 00 04 E9 88 27 40 00 04 20 2B 00 08 E9 88 27
```
As this code is never executed by original game's packer, patching this code will not affect game anyhow. Note, that files, compressed with this version of tool will be compatible only with patched ROM. And decompression will function properly with any version of compressor.
Below are compression modes with short descriptions:  
```
0X - rle. repeat previous scanline X+1 times
1X - oneColor. Fill scanline with X color
2X - mirrorBack. Mirror scanline X+1 scanlines back in output
3X YY - mirrorBackExt. Mirror scanline XYY+1 scanlines back in output
4X CC CC CC CC ... - raw. Copy next X+1 scanlines from input
5X MM - setColorByMask. Set pixels marked with 1 in MM bitmask to X color. 0 places are left as they were in preivious scanline.
6R CL MM - set3ColorsByMask. Mask is read right to left. Set R color to first right raised bit, then C to center, then L to left. 
7X - shiftL. Shift scanline left one pixel, set came free right color to X.
8X - shiftR. Shift scanline right one pixel, set came free left color to X.
9R LC - build1Border. Build scanline from right to left: repeat C times R color and all left colors set to L
AR ML CK - build2Border. Build scanline from right to left: repeat C times R color, then K times M color and all left colors set to L
BX - extension. Set specified pairs of pixels
	B0 XY - set1. Set XYCCCCCC, where CCCCCC - previous scanline data
	B1 XY - set2. Set CCXYCCCC
	B2 XY - set3. Set CCCCXYCC
	B3 XY - set4. Set CCCCCCXY
	B4 XY WZ - set12. Set XYWZCCCC
	B5 XY WZ - set13. Set XYCCWZCC
	B6 XY WZ - set14. Set XYCCCCWZ
	B7 XY WZ - set23. Set CCXYWZCC
	B8 XY WZ - set24. Set CCXYCCWZ
	B9 XY WZ - set34. Set CCCCXYWZ
	BA-BF - copy. Just save CCCCCCCC
CX-FF - next. Read next command.
```
Build with [Haskell Stack](https://haskellstack.org) tool.
