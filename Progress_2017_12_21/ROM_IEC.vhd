----------------------------------------------------------------------------------
--  Load an embbedded PRG file in block RAM to Zybo C64 via IEC
----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity ROM_IEC is
port (
    ATN0        :   in  std_logic;          -- master broadcasting command when low
    LCLK0       :   in  std_logic;          -- input clock when listening 0=low=idle 1=bit valid
    DATAI0      :   in  std_logic;          -- input data when listening
    TCLK0       :   out std_logic;          -- output clock when talking  0=low=idle 1=bit valid
    DATAO0      :   out std_logic;          -- output data when talking

    clk         : in std_logic              -- system clock (use PH2)
);
end ROM_IEC;

architecture IEC_ROMLoad_Impl of ROM_IEC is

signal atn          : std_logic;
signal clkr         : std_logic;
signal datar        : std_logic;
signal clkw         : std_logic:='1';
signal dataw        : std_logic:='1';

signal clk_pull     : std_logic;
signal data_pull    : std_logic;

subtype u4      is unsigned(3 downto 0);
subtype nybble  is std_logic_vector(3 downto 0);
subtype u8      is unsigned(7 downto 0);
subtype byte    is std_logic_vector(7 downto 0);
subtype u16     is unsigned(15 downto 0);
subtype word    is std_logic_vector(15 downto 0);

constant    prg_size    : u16 := x"66BF";
signal      prg_addr    : u16 := x"0000";

type iec_st is (
    iec_idle
);
signal iecST    : iec_st    := iec_idle;
signal iecSeq   : byte      := x"00";
signal iecFNp   : nybble    := x"0";
signal iecCyc   : byte      := x"00";
signal iecEOI   : std_logic := '0';
signal xferSR   : byte      := x"00";       -- shift reg for xfers
signal xferCnt  : u4        := x"0";
signal xferTrn  : std_logic;

begin

atn     <= ATN0;
clkr    <= LCLK0;
datar   <= DATAI0;
TCLK0   <= clkw;
DATAO0  <= dataw;

clk_pull    <= clkw and not LCLK0;      -- 1 whenever we output a one but signal is in pulldown
data_pull   <= dataw and not DATAI0;    -- 1 whenever we output a one but signal is in pulldown

iec_proto: process(clk,iecST,iecSeq,iecFNp,iecCyc,iecEOI,atn,clkr,datar) is
variable eoi : std_logic;
variable cyc : u8;
variable seq : u8;
variable fnp : u4;
begin
    if (rising_edge(clk)) then
        seq := u8(iecSeq);
        fnp := u4(iecFNp);
        cyc := u8(iecCyc);
        eoi := iecEOI;
        case iecST is
            when iec_idle                   =>
                clkw <= '1';
                dataw <= '1';
            when others                     =>
                null;
        end case;
        iecSeq <= byte(seq);
        iecFNp <= nybble(fnp);
        iecCyc <= byte(cyc);
        iecEOI <= eoi;
    end if;
end process iec_proto;

end IEC_ROMLoad_Impl;
