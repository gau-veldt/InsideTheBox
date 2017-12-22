#! /usr/bin/python

import sys,os,os.path

romFile="futurewriter.prg"
outFile="futurewriter_prg.vhd"
fnName="futurewriter_prg"

rom = open(romFile)
romSz = os.path.getsize(romFile)
romImage = rom.read(romSz)
print "PRG is 0x{0:04x} ({0:d}) bytes".format(romSz)

src= open(outFile,"w")

def libs():
	src.write("library IEEE;\n")
	src.write("use IEEE.std_logic_1164.all;\n\n")

def subtypes():
	src.write("subtype word is std_logic_vector(15 downto 0);\n")
	src.write("subtype u16 is unsigned(15 downto 0);\n")
	src.write("subtype slv13 is std_logic_vector(12 downto 0);\n")
	src.write("subtype byte is std_logic_vector(7 downto 0);\n\n")

libs()

src.write("package p_{0} is\n\n".format(fnName))

subtypes()

dollar = chr(1)+chr(8)+chr(14)+chr(8)+chr(100)+chr(0)+("\"{0:5d} *\"".format(romSz))+chr(0)+chr(0)+chr(0)

src.write("function {0}dir(addr : word) return byte;\n".
	format(fnName[:-3]))
src.write("constant {0}dir_len : word := x\"{1:04x}\";\n\n".format(fnName[:-3],len(dollar)))
src.write("function {0}(addr : word) return byte;\n".
	format(fnName))
src.write("constant {0}_len : word := x\"{1:04x}\";\n\n".format(fnName,romSz))
src.write("end package p_{0};\n\n".format(fnName))

src.write("package body p_{0} is\n\n".format(fnName))

src.write("function {0}dir(addr : word) return byte is\n".
	format(fnName[:-3]))
src.write("begin\n")
src.write("    case addr is\n")

for addr in range(len(dollar)):
	byte=ord(dollar[addr])
	src.write("        when x\"{0:04x}\" => return x\"{1:02x}\";\n".format(addr,byte))

src.write("        when others          => return x\"00\";\n")
src.write("    end case;\n")
src.write("end {0}dir;\n\n".format(fnName[:-3]))

src.write("function {0}(addr : word) return byte is\n".
	format(fnName))
src.write("begin\n")
src.write("    case addr is\n")

for addr in range(romSz):
	byte=ord(romImage[addr])
	src.write("        when x\"{0:04x}\" => return x\"{1:02x}\";\n".format(addr,byte))

src.write("        when others          => return x\"00\";\n")
src.write("    end case;\n")
src.write("end {0};\n\n".format(fnName))
src.write("end package body p_{0};\n\n".format(fnName))

