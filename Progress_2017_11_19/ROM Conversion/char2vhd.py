#! /usr/bin/python

import sys,os,os.path

romFile="Char ROM"
outFile="CharROM.vhd"
fnName="char_rom"

rom = open(romFile)
romSz = os.path.getsize(romFile)
romImage = rom.read(romSz)
print "ROM is 0x{0:04x} ({0:d}) bytes".format(romSz)

src= open(outFile,"w")

def libs():
	src.write("library IEEE;\n")
	src.write("use IEEE.std_logic_1164.all;\n\n")

def subtypes():
	src.write("subtype slv12 is std_logic_vector(11 downto 0);\n")
	src.write("subtype byte is std_logic_vector(7 downto 0);\n\n")

libs()

src.write("package p_{0} is\n\n".format(fnName))

subtypes()

src.write("function {0}(addr : slv12) return byte;\n".
	format(fnName))
src.write("end package p_{0};\n\n".format(fnName))

src.write("package body p_{0} is\n\n".format(fnName))

src.write("function {0}(addr : slv12) return byte is\n".
	format(fnName))
src.write("begin\n")
src.write("    case addr is\n")

for addr in range(romSz):
	byte=ord(romImage[addr])
	src.write("        when x\"{0:03x}\" => return \"{1:08b}\";\n".format(addr,byte))

src.write("        when others => return \"00000000\";\n")
src.write("    end case;\n")
src.write("end {0};\n\n".format(fnName))
src.write("end package body p_{0};\n\n".format(fnName))

