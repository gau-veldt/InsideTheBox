----------------------------------------------------------------------------------
--
--  Commodore 64 on Zybo
-- 
----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

--
-- C64 ROM includes (NOT INCLUDED)
-- See python scripts for converting ROM images to VHDL files.
-- Import the generated sources into a library called "c64roms"
--
library c64roms;
use c64roms.p_char_rom.all;     -- for   char_rom(addr[11:0]) return byte[7:0]
use c64roms.p_basic_rom.all;    -- for  basic_rom(addr[12:0]) return byte[7:0]
use c64roms.p_kernal_rom.all;   -- for kernal_rom(addr[12:0]) return byte[7:0]

entity chip_test is
port (
    clk_125     :   in      std_logic;

    -- audio configure
    ac_scl      :   inout   std_logic;
    ac_sda      :   inout   std_logic;

    -- audio signal
    ac_muten    :   out     std_logic;
    ac_mclk     :   out     std_logic;
    ac_bclk     :   out     std_logic;
    ac_pbdat    :   out     std_logic;
    ac_pblrc    :   out     std_logic;
    ac_recdat   :   in      std_logic;
    ac_reclrc   :   in      std_logic;
    -- report error in configuring audio
    led         :   out     std_logic_vector(3 downto 0);
    -- to test waveforms
    sw          :   in      std_logic_vector(3 downto 0);
    -- for freq/wvfm ramping tests
    btn         :   in      std_logic_vector(3 downto 0);

    vga_hs      :   out     std_logic;
    vga_vs      :   out     std_logic;
    vga_r       :   out     std_logic_vector(4 downto 0);
    vga_g       :   out     std_logic_vector(5 downto 0);
    vga_b       :   out     std_logic_vector(4 downto 0)
);
end chip_test;

architecture testing of chip_test is

component clk_wiz_0
port
 (-- Clock in ports
  clk_in1           : in     std_logic;
  -- Clock out ports
  clk160            : out    std_logic;
  clk20ph1          : out    std_logic;
  clk20ph2          : out    std_logic;
  clk20ph3          : out    std_logic;
  clk20ph4          : out    std_logic;
  -- Status and control signals
  reset             : in     std_logic;
  locked            : out    std_logic
 );
end component;
component clk_wiz_1
port
 (-- Clock in ports
  clk_in1           : in     std_logic;
  -- Clock out ports
  clk12             : out    std_logic;
  -- Status and control signals
  reset             : in     std_logic;
  locked            : out    std_logic
 );
end component;
signal clk12        : std_logic;
signal clk160       : std_logic;
signal clk_lk1      : std_logic;
signal clk_lk2      : std_logic;
signal clk20p000    : std_logic;
signal clk20p010    : std_logic;
signal clk20p100    : std_logic;
signal clk20p110    : std_logic;
signal clk20_ph1    : std_logic;
signal clk20_ph2    : std_logic;

component blk_ram_64k
  port (
    clka : IN STD_LOGIC;
    rsta : IN STD_LOGIC;
    ena : IN STD_LOGIC;
    wea : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    addra : IN STD_LOGIC_VECTOR(15 DOWNTO 0);
    dina : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
    douta : OUT STD_LOGIC_VECTOR(7 DOWNTO 0)
  );
end component;

component blk_cram
  port (
    clka : IN STD_LOGIC;
    rsta : IN STD_LOGIC;
    ena : IN STD_LOGIC;
    wea : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    addra : IN STD_LOGIC_VECTOR(9 DOWNTO 0);
    dina : IN STD_LOGIC_VECTOR(3 DOWNTO 0);
    douta : OUT STD_LOGIC_VECTOR(3 DOWNTO 0)
  );
end component;

subtype pair is std_logic_vector(1 downto 0);
subtype slv3 is std_logic_vector(2 downto 0);
subtype nybble is std_logic_vector(3 downto 0);
subtype slv5 is std_logic_vector(4 downto 0);
subtype u5 is unsigned(4 downto 0);
subtype u6 is unsigned(5 downto 0);
subtype slv6 is std_logic_vector(5 downto 0);
subtype slv7 is std_logic_vector(6 downto 0);
subtype byte is std_logic_vector(7 downto 0);
subtype ubyte is unsigned(7 downto 0);
subtype slv9 is std_logic_vector(8 downto 0);
subtype slv10 is std_logic_vector(9 downto 0);
subtype slv14 is std_logic_vector(13 downto 0);
subtype word is std_logic_vector(15 downto 0);
subtype u16 is unsigned(15 downto 0);
subtype s16 is signed(15 downto 0);
subtype slv17 is std_logic_vector(16 downto 0);
subtype slv20 is std_logic_vector(19 downto 0);
subtype slv24 is std_logic_vector(23 downto 0);
subtype slv27 is std_logic_vector(26 downto 0);     -- I2C SSM2603 send with ACK pads

signal res0 : std_logic;
signal res1 : std_logic;

signal cpu_r1w0 : std_logic:='1';
signal cpu_r0w1 : std_logic;
signal cpu_run  : std_logic:='0';
signal cpu_rclk : std_logic;

component chip6502 is
port (
    a       :   out     std_logic_vector(15 downto 0);
    di      :   in      std_logic_vector(7 downto 0);
    do      :   out     std_logic_vector(7 downto 0);
    pi      :   in      std_logic_vector(7 downto 0);
    po      :   out     std_logic_vector(7 downto 0);
    r1w0    :   out     std_logic;
    sync    :   out     std_logic;
    nmi0    :   in      std_logic;
    irq0    :   in      std_logic;
    so0     :   in      std_logic;        
    rdy     :   in      std_logic;
    res0    :   in      std_logic;
    ph4Xin  :   in      std_logic;  -- clock input
    ph0     :   out     std_logic;
    ph1     :   out     std_logic;  -- clock on high edge
    ph2     :   out     std_logic   -- clock on low edge
);
end component;
signal c6510_pi    : byte;
signal c6510_po    : byte;
signal c6510_a     : word;
signal c6510_sync  : std_logic;
signal c6510_rdy   : std_logic;
signal c6510_irq0  : std_logic;
signal c6510_nmi0  : std_logic;
signal c6510_so0   : std_logic;

component sid6581 is
port (
    res0        :   in  std_logic;
    ph2         :   in  std_logic;
    rga         :   in  std_logic_vector(4 downto 0);
    din         :   in  std_logic_vector(7 downto 0);
    dout        :   out std_logic_vector(7 downto 0);
    r1w0        :   in  std_logic;
    s16audio    :   out signed(15 downto 0)
);
end component;

signal bclk_cnt : pair;
alias bclk_ref : std_logic is bclk_cnt(0);--1
signal audio_frame : u6;
signal hold_srl : word;

function isZero6(tst: u6) return boolean is
begin
    case tst is
        when "000000"   => return true;
        when others     => return false;
    end case;
end;

function adv_frame(frm: u6) return u6 is
begin
    case frm is
        when "000000"   => return "000001";
        when "000001"   => return "000010";
        when "000010"   => return "000011";
        when "000011"   => return "000100";
        when "000100"   => return "000101";
        when "000101"   => return "000110";
        when "000110"   => return "000111";
        when "000111"   => return "001000";
        when "001000"   => return "001001";
        when "001001"   => return "001010";
        when "001010"   => return "001011";
        when "001011"   => return "001100";
        when "001100"   => return "001101";
        when "001101"   => return "001110";
        when "001110"   => return "001111";
        when "001111"   => return "010000";
        when "010000"   => return "010001";
        when "010001"   => return "010010";
        when "010010"   => return "010011";
        when "010011"   => return "010100";
        when "010100"   => return "010101";
        when "010101"   => return "010110";
        when "010110"   => return "010111";
        when "010111"   => return "011000";
        when "011000"   => return "011001";
        when "011001"   => return "011010";
        when "011010"   => return "011011";
        when "011011"   => return "011100";
        when "011100"   => return "011101";
        when "011101"   => return "011110";
        when "011110"   => return "011111";
        when "011111"   => return "000000";                     -- counts from 0 to 31
        when "100000"   => return "100001";
        when "100001"   => return "100010";
        when "100010"   => return "100011";
        when "100011"   => return "100100";
        when "100100"   => return "100101";
        when "100101"   => return "100110";
        when "100110"   => return "100111";
        when "100111"   => return "101000";
        when "101000"   => return "101001";
        when "101001"   => return "101010";
        when "101010"   => return "101011";
        when "101011"   => return "101100";
        when "101100"   => return "101101";
        when "101101"   => return "101110";
        when "101110"   => return "101111";
        when "101111"   => return "110000";
        when "110000"   => return "110001";
        when "110001"   => return "110010";
        when "110010"   => return "110011";
        when "110011"   => return "110100";
        when "110100"   => return "110101";
        when "110101"   => return "110110";
        when "110110"   => return "110111";
        when "110111"   => return "111000";
        when "111000"   => return "111001";
        when "111001"   => return "111010";
        when "111010"   => return "111011";
        when "111011"   => return "111100";
        when "111100"   => return "111101";
        when "111101"   => return "111110";
        when "111110"   => return "111111";
        when "111111"   => return "000000";
        when others     => return "000000";
    end case;
end adv_frame;

signal sid1_rga     : slv5;
signal sid1_dw      : byte;
signal sid1_dr      : byte;
signal sid1_out     : s16;
signal sid1_r1w0    : std_logic;

component vic_ii is
port (
    -- register access
    rga         : in      std_logic_vector(5 downto 0);
    rgdi        : in      std_logic_vector(7 downto 0);
    rgdo        : out     std_logic_vector(7 downto 0);
    r1w0        : in      std_logic;
    -- video access
    va          : out     std_logic_vector(13 downto 0);
    vd          : in      std_logic_vector(7 downto 0);
    cd          : in      std_logic_vector(3 downto 0);
    -- bus mastering
    cpu_clk     : out     std_logic;                            -- 4 MHz CPU clock
    cpu_ben     : out     std_logic;                            -- 1=CPU on buses
    vic_ben     : out     std_logic;                            -- 1=VIC on buses
    bus_ph0     : out     std_logic;                            -- master PH0 clock
    bus_ph1     : out     std_logic;                            -- master PH1 clock
    bus_ph2     : out     std_logic;                            -- master PH2 clock
    res0        : in      std_logic;                            -- reset (low)
    -- external signals
    clk20_ph1   : in      std_logic;
    clk20_ph2   : in      std_logic;
    vhs         : out     std_logic;
    vvs         : out     std_logic;
    vr          : out     std_logic_vector(4 downto 0);
    vg          : out     std_logic_vector(5 downto 0);
    vb          : out     std_logic_vector(4 downto 0)
);
end component;

signal bankctl      : slv5 :=  "11111";
alias exrom         : std_logic is bankctl(4);
alias game          : std_logic is bankctl(3);
alias charen        : std_logic is bankctl(2);
alias hiram         : std_logic is bankctl(1);
alias loram         : std_logic is bankctl(0);

signal abus         : word;
signal rama         : word;
signal cpudo        : byte;
signal cpudi        : byte;
signal r1w0         : std_logic;
signal ram_clk      : std_logic;
signal ramclk_init  : std_logic := '0';
signal ramclk_cpu   : std_logic := '0';
signal ramclk_vic   : std_logic := '0';
signal ram_r1w0     : std_logic := '1';
signal ram_r0w1     : std_logic;
signal cram_r1w0    : std_logic := '1';
signal cram_r0w1    : std_logic;
signal ramdr        : byte;
signal ramdw        : byte;
signal ramen        : std_logic := '1';
signal cramdr       : nybble;
signal cramdw       : nybble;
signal cramen       : std_logic := '1';
signal vbank        : pair := "00";
signal vic_rga      : slv6;
signal vic_regw     : byte;
signal vic_regr     : byte;
signal vic_r1w0     : std_logic := '1';
signal cpu_ph1      : std_logic;
signal cpu_ph2      : std_logic;
signal vmem_clk     : std_logic;
signal vic_ca       : slv10;
signal vic_cd       : nybble;
signal vic_wcd      : nybble;
signal vic_va       : slv14;
signal vic_vd       : byte;
signal vic_wvd      : byte;
signal cpu_on       : std_logic;
signal vic_on       : std_logic;
signal cpu_ph4x     : std_logic;

function catoi(src: slv10) return integer is
begin
    return to_integer(unsigned(src));
end catoi;

function vatoi(src: word) return integer is
begin
    return to_integer(unsigned(src));
end vatoi;

function xtov6(src: byte) return slv6 is
begin
    return src(5 downto 0);
end xtov6;

function xtov5(src: byte) return slv5 is
begin
    return src(4 downto 0);
end xtov5;

type mbank_t is (
    mbk_ram,
    mbk_cram,
    mbk_lorom,
    mbk_hirom,
    mbk_xio2,
    mbk_xio1,
    mbk_cia2,
    mbk_cia1,
    mbk_cgrom,
    mbk_sid,
    mbk_vic
);

function init_bank(addr:u16) return mbank_t is
begin
    if (addr(15 downto 10) = "110110") then
        return mbk_cram;
    else
        return mbk_ram;
    end if;
end init_bank;
signal cur_a_bank   : mbank_t;
signal cur_a_wt     : std_logic;

function cpu_bank(addr: u16; bsel: slv5) return mbank_t is
variable b3 : slv3;
variable bb0 : std_logic;
variable ce : std_logic;
variable n3 : nybble;
begin
    b3 := bsel(2 downto 0);
    bb0 := bsel(1) nor bsel(0);
    ce := bsel(2);
    n3 := nybble(addr(11 downto 8));
    if (addr >= x"e000") then
        if (bsel(1)='1') then
            return mbk_hirom;
        else
            return mbk_ram;
        end if;
    elsif (addr >= x"d000" and addr < x"e000") then
        if (bb0 = '1') then
            return mbk_ram;
        else
            if (ce = '0') then
                return mbk_cgrom;
            else
                case n3 is
                    when x"f" =>
                        return mbk_xio2;
                    when x"e" =>
                        return mbk_xio1;
                    when x"d" =>
                        return mbk_cia2;
                    when x"c" =>
                        return mbk_cia1;
                    when x"b" =>
                        return mbk_cram;
                    when x"a" =>
                        return mbk_cram;
                    when x"9" =>
                        return mbk_cram;
                    when x"8" =>
                        return mbk_cram;
                    when x"7" =>
                        return mbk_sid;
                    when x"6" =>
                        return mbk_sid;
                    when x"5" =>
                        return mbk_sid;
                    when x"4" =>
                        return mbk_sid;
                    when x"3" =>
                        return mbk_vic;
                    when x"2" =>
                        return mbk_vic;
                    when x"1" =>
                        return mbk_vic;
                    when x"0" =>
                        return mbk_vic;
                    when others =>
                        return mbk_ram;
                end case;                
            end if;
        end if;
    elsif (addr >= x"a000" and addr < x"c000") then
        if ((bsel(1) and bsel(0)) = '1') then
            return mbk_lorom;
        else
            return mbk_ram;
        end if;
    else
        return mbk_ram;
    end if;
end cpu_bank;

function bank_is_writethru(addr:word; bsel:slv5) return std_logic is
begin
    case cpu_bank(u16(addr),bsel) is
        when mbk_ram    => return '1';
        when mbk_cram   => return '1';
        when mbk_lorom  => return '1';
        when mbk_hirom  => return '1';
        when others     => return '0';
    end case;
end bank_is_writethru;

type init_t is (
    initram_wait, initram_xfer, initram_idle);
signal init_stg : init_t := initram_wait;
constant init_wait_to : u16 := x"0014";
signal init_counter : u16 := x"0000";
signal init_addr : word;
signal init_data : byte;
signal init_r1w0 : std_logic := '1';
signal init_r0w1 : std_logic;
function ram_init(lin: u16) return slv24 is
begin
    case lin is
        when x"0000"    => return x"0400" & x"08";
        when x"0001"    => return x"0401" & x"05";
        when x"0002"    => return x"0402" & x"0c";
        when x"0003"    => return x"0403" & x"0c";
        when x"0004"    => return x"0404" & x"0f";
        when x"0005"    => return x"0405" & x"2c";
        when x"0006"    => return x"0406" & x"20";
        when x"0007"    => return x"0407" & x"17";
        when x"0008"    => return x"0408" & x"0f";
        when x"0009"    => return x"0409" & x"12";
        when x"000a"    => return x"040a" & x"0c";
        when x"000b"    => return x"040b" & x"04";
        when x"000c"    => return x"040c" & x"21";
        when x"000d"    => return x"040d" & x"20";
        when x"000e"    => return x"05f2" & x"3a";
        when x"000f"    => return x"05f3" & x"3a";
        when x"0010"    => return x"05f4" & x"3a";
        when x"0011"    => return x"05f5" & x"3a";
        when x"0012"    => return x"07de" & x"30";
        when x"0013"    => return x"07df" & x"31";
        when x"0014"    => return x"07e0" & x"32";
        when x"0015"    => return x"07e1" & x"33";
        when x"0016"    => return x"07e2" & x"34";
        when x"0017"    => return x"07e3" & x"35";
        when x"0018"    => return x"07e4" & x"36";
        when x"0019"    => return x"07e5" & x"37";
        when x"001a"    => return x"07e6" & x"38";
        when x"001b"    => return x"07e7" & x"39";
        when x"001c"    => return x"d800" & x"01";
        when x"001d"    => return x"d801" & x"02";
        when x"001e"    => return x"d802" & x"03";
        when x"001f"    => return x"d803" & x"04";
        when x"0020"    => return x"d804" & x"05";
        when x"0021"    => return x"d805" & x"07";
        when x"0022"    => return x"d806" & x"0e";
        when x"0023"    => return x"d807" & x"08";
        when x"0024"    => return x"d808" & x"09";
        when x"0025"    => return x"d809" & x"0a";
        when x"0026"    => return x"d80a" & x"0b";
        when x"0027"    => return x"d80b" & x"0c";
        when x"0028"    => return x"d80c" & x"0d";
        when x"0029"    => return x"d80d" & x"0e";
        when x"002a"    => return x"d9f2" & x"01";
        when x"002b"    => return x"d9f3" & x"01";
        when x"002c"    => return x"d9f4" & x"01";
        when x"002d"    => return x"d9f5" & x"01";
        when x"002e"    => return x"dbde" & x"05";
        when x"002f"    => return x"dbdf" & x"07";
        when x"0030"    => return x"dbe0" & x"0d";
        when x"0031"    => return x"dbe1" & x"09";
        when x"0032"    => return x"dbe2" & x"08";
        when x"0033"    => return x"dbe3" & x"0a";
        when x"0034"    => return x"dbe4" & x"0b";
        when x"0035"    => return x"dbe5" & x"0c";
        when x"0036"    => return x"dbe6" & x"0f";
        when x"0037"    => return x"dbe7" & x"0e";
        when others     => return x"0000" & x"00";      -- 0 (and 1) inaccessible on C64
    end case;
end ram_init; 
constant ram_init_count : u16 := x"0038";

-- for initial settings for vic/sid
signal c_cycle          :   ubyte := x"00";

--
-- For setting the SSM2603 via I2C
--
function dac_init(lin : ubyte) return slv20 is
begin
    case lin is
        when x"00"      => return x"06" & x"010";
        when x"01"      => return x"02" & x"075";
        when x"02"      => return x"03" & x"075";
        when x"03"      => return x"04" & x"010";
        when x"04"      => return x"05" & x"000";
        when x"05"      => return x"07" & x"003";
        when x"06"      => return x"08" & x"000";
        when x"07"      => return x"09" & x"001";
        when x"08"      => return x"06" & x"000";
        when others     => return x"ff" & x"fff";
    end case;
end dac_init;

component i2c_xcvr port
(
    -- with 1 MHz clock SSM2603 reg 9 delay step is 65.535 ms:
    clk1M       :   in      std_logic;
    
    -- tie to system reset
    res0        :   in      std_logic;
    
    -- initialization line number
    init_line   :   out     unsigned(7 downto 0);
    -- initialization data xRRRRRRRxxxxxxxDDDDDDDDD
    init_data   :   in      std_logic_vector(19 downto 0);
    
    -- error status
    error       :   out     std_logic;
    
    -- tie directly to ac_scl and ac_sda accordingly:
    scl         :   inout   std_logic;
    sda         :   inout   std_logic
);
end component;
signal ssm_init_line    :   ubyte;
signal ssm_init_data    :   slv20;
signal ssm_error        :   std_logic;

-- 3-bit counter for dividing 160 MHz into 8-phase 20 MHz clocks
signal clk20_ph : slv3;
function c20_next(src: slv3) return slv3 is
begin
    case src is
        when "000"  => return "001";
        when "001"  => return "010";
        when "010"  => return "011";
        when "011"  => return "100";
        when "100"  => return "101";
        when "101"  => return "110";
        when "110"  => return "111";
        when "111"  => return "000";
        when others => return "000";
    end case;
end c20_next;

function vbk_cgrom(bk: pair; adr: slv14) return std_logic is
variable vsel : slv3;
begin
    vsel := bk(0) & adr(13 downto 12);
    case vsel is
        when "001"  => return '1';
        when others => return '0';
    end case;
end vbk_cgrom;

subtype u12 is unsigned(11 downto 0);
signal reset_wait : u12 := x"000";
constant reset_delay : u12 := x"0fb";       -- number of 125 MHz clocks in 2000 ns

signal cgrom_data : byte;
signal lorom_data : byte;
signal hirom_data : byte;

--
--  Architectural implementation
--
begin

--
--  Coldstart reset for 2000 ns
--
initial_reset: process(clk_125, reset_wait) is              -- counts up to 2000 ns
variable not_yet : boolean;
begin
    not_yet := (reset_wait < reset_delay);
    if rising_edge(clk_125) then
        if not_yet then
            reset_wait <= reset_wait + 1;
        end if;
    end if;
    if not_yet then                             -- drive reset accordingly
        res0 <= '0';
    else
        res0 <= '1';
    end if;
end process initial_reset;

pixclock: clk_wiz_0 port map ( 
    -- Clock out ports  
    clk160 => clk160,
    clk20ph1 => clk20p010,
    clk20ph2 => clk20p100,
    clk20ph3 => clk20p110,
    clk20ph4 => clk20p000,
    -- Status and control signals                
    reset => res1,
    locked => clk_lk1,
    -- Clock in ports
    clk_in1 => clk_125
);

clk20gen: process(clk160) is
--clk20gen: process(clk20p000,clk20p010,clk20p100,clk20p110) is
variable cnext : slv3;
begin
    if (rising_edge(clk160)) then
        cnext := c20_next(clk20_ph);
        clk20_ph <= cnext;
        clk20_ph1 <= cnext(2) and (cnext(1) nor cnext(0));
        clk20_ph2 <= not (cnext(2) or cnext(1) or cnext(0));
    end if;
    --if ( rising_edge(clk20p000)) then
    --    clk20_ph <= "000";
    --end if;
    --if (falling_edge(clk20p000)) then
    --    clk20_ph <= "001";
    --end if;
    --if ( rising_edge(clk20p010)) then
    --    clk20_ph <= "010";
    --end if;
    --if (falling_edge(clk20p010)) then
    --    clk20_ph <= "011";
    --end if;
    --if ( rising_edge(clk20p100)) then
    --    clk20_ph <= "100";
    --end if;
    --if (falling_edge(clk20p100)) then
    --    clk20_ph <= "101";
    --end if;
    --if ( rising_edge(clk20p110)) then
    --    clk20_ph <= "110";
    --end if;
    --if (falling_edge(clk20p110)) then
    --    clk20_ph <= "111";
    --end if;
end process clk20gen;
--clk20_ph1 <= clk20p100;
--clk20_ph2 <= clk20p000;

cgrom_latch: process(rama,ram_clk) is
begin
    if rising_edge(ram_clk) then
        cgrom_data <= char_rom(rama(11 downto 0));
    end if;
end process cgrom_latch;

lorom_latch: process(rama,ram_clk) is
begin
    if rising_edge(ram_clk) then
        lorom_data <= basic_rom(rama(12 downto 0));
    end if;
end process lorom_latch;

hirom_latch: process(rama,ram_clk) is
begin
    if rising_edge(ram_clk) then
        hirom_data <= kernal_rom(rama(12 downto 0));
    end if;
end process hirom_latch;

cpu: chip6502 port map (
    a           => c6510_a,
    di          => cpudi,
    do          => cpudo,
    r1w0        => cpu_r1w0,
    pi          => c6510_pi,
    po          => c6510_po,
    irq0        => c6510_irq0,
    nmi0        => c6510_nmi0,
    so0         => c6510_so0,
    rdy         => c6510_rdy,
    ph4xin      => cpu_rclk,
    res0        => res0
);
c6510_irq0 <= '1';
c6510_nmi0 <= '1';
c6510_so0 <= '1';
c6510_rdy <= '1';
cpu_rclk <= cpu_ph4x and cpu_run;

init_ram: process(clk160,clk20_ph,init_stg) is
begin
    if rising_edge(clk160) then
        if (init_stg = initram_xfer and (clk20_ph >= "110" and clk20_ph <= "111")) then
            init_r1w0 <= '0';
        else
            init_r1w0 <= '1';
        end if;
        if (clk20_ph = "011") then
            case init_stg is
                when initram_wait =>
                    if (init_counter = init_wait_to) then
                        init_counter <= x"0000";
                        init_stg <= initram_xfer;
                        init_addr <= x"ffff";
                        init_data <= x"ff";
                    else
                        init_counter <= init_counter + 1;
                    end if;
                when initram_xfer =>
                    if (init_counter = ram_init_count) then
                        init_counter <= x"0000";
                        init_stg <= initram_idle;
                    else
                        init_addr <= ram_init(init_counter)(23 downto 8);
                        init_data <= ram_init(init_counter)(7 downto 0);
                        init_counter <= init_counter + 1;
                    end if;
                when others =>
                    null;
            end case;
        end if;
    end if;
end process init_ram;

sndclock: clk_wiz_1 port map ( 
    -- Clock out ports  
    clk12 => clk12,
    -- Status and control signals                
    reset => res1,
    locked => clk_lk2,
    -- Clock in ports
    clk_in1 => clk_125
);

ram64: blk_ram_64k port map(
    clka        => ram_clk,
    addra       => rama,
    dina        => ramdw,
    douta       => ramdr,
    wea(0)      => ram_r0w1,
    rsta        => res1,
    ena         => ramen
);

color_ram: blk_cram port map(
    clka        => ram_clk,
    addra       => rama(9 downto 0),
    dina        => cramdw,
    douta       => cramdr,
    wea(0)      => cram_r0w1,
    rsta        => res1,
    ena         => cramen
);

res1 <= not res0;
cpu_r0w1 <= not cpu_r1w0;
ram_r0w1 <= not ram_r1w0;
cram_r0w1 <= not cram_r1w0;
init_r0w1 <= not init_r1w0;

ram_wren: process(cpu_r1w0,abus,bankctl,init_counter,init_addr,init_r1w0,init_stg,cpu_on) is
begin
    if (init_stg = initram_xfer and init_counter > x"0000") then
        if (init_bank(u16(init_addr)) = mbk_cram) then
            cram_r1w0 <= init_r1w0;
            ram_r1w0 <= '1';
        else
            cram_r1w0 <= '1';
            ram_r1w0 <= init_r1w0;
        end if;
    elsif (cpu_on = '1') then
        if (cpu_bank(u16(abus),bankctl) = mbk_cram) then
            cram_r1w0 <= cpu_r1w0;
            ram_r1w0 <= '1';
        else
            if (bank_is_writethru(abus,bankctl)='1') then
                cram_r1w0 <= '1';
                ram_r1w0 <= cpu_r1w0;
            else
                cram_r1w0 <= '1';
                ram_r1w0 <= '1';
            end if;
        end if;
    else
        cram_r1w0 <= '1';
        ram_r1w0 <= '1';
    end if;
end process ram_wren;

with init_stg select ramdw <=
    init_data   when initram_xfer,
    cpudo        when others;

with init_stg select cramdw <=
        init_data(3 downto 0)   when initram_xfer,
        cpudo(3 downto 0)        when others;

abus <= c6510_a;
cur_a_bank <= cpu_bank(u16(abus),bankctl);
cur_a_wt <= bank_is_writethru(abus,bankctl);

--  CPU read data select
--  TODO: mbk_xio2,mbk_xio1,mbk_cia2,mbk_cia1
with cur_a_bank select cpudi <=
    ramdr                           when mbk_ram,
    ramdr(7 downto 4) & cramdr      when mbk_cram,
    vic_regr                        when mbk_vic,
    sid1_dr                         when mbk_sid,
    --char_rom(abus(11 downto 0))     when mbk_cgrom,
    cgrom_data                      when mbk_cgrom,
    lorom_data                      when mbk_lorom,
    hirom_data                      when mbk_hirom,
    ramdr                           when others;

ram_a_sel: process(init_stg,cpu_on,init_addr,abus,vbank,vic_va) is
begin
    if (init_stg = initram_xfer) then
        rama <= init_addr;
    elsif (cpu_on = '1') then
        rama <= abus;
    else
        rama <= vbank & vic_va;
    end if;
end process ram_a_sel;

ram_clk_sel: process(init_stg,cpu_on,ramclk_init,ramclk_cpu,ramclk_vic) is
begin
    if (init_stg = initram_xfer) then
        ram_clk <= ramclk_init;
    elsif (cpu_on = '1') then
        ram_clk <= ramclk_cpu;
    else
        ram_clk <= ramclk_vic;
    end if;
end process ram_clk_sel;

with clk20_ph select ramclk_init <=
    '1' when "110",
    '1' when "000",
    '0' when others;
with clk20_ph select ramclk_vic <=
    '1' when "110",
    '1' when "000",
    '0' when others;
with clk20_ph select ramclk_cpu <=
    '1' when "110",
    '1' when "000",
    '0' when others;

vic: vic_ii port map(
    clk20_ph1   => clk20_ph1,
    clk20_ph2   => clk20_ph2,
    rga         => vic_rga,
    rgdi        => vic_regw,
    rgdo        => vic_regr,
    r1w0        => vic_r1w0,
    cpu_clk     => cpu_ph4x,
    cpu_ben     => cpu_on,
    vic_ben     => vic_on,
    bus_ph1     => cpu_ph1,
    bus_ph2     => cpu_ph2,
    va          => vic_va,
    vd          => vic_vd,
    cd          => vic_cd,
    res0        => res0,
    vhs         => vga_hs,
    vvs         => vga_vs,
    vr          => vga_r,
    vg          => vga_g,
    vb          => vga_b
);

vic_vd_sel: process(vbank,vic_va,ramdr) is
begin
    if (vbk_cgrom(vbank,vic_va) = '1') then
        --vic_wvd <= char_rom(vic_va(11 downto 0));
        vic_wvd <= cgrom_data;
    else
        vic_wvd <= ramdr;
    end if;
end process vic_vd_sel;
    
vic_wcd <= cramdr;

vic_vd <= vic_wvd;
vic_cd <= vic_wcd;

cpu_start: process(cpu_on,cpu_ph1,c_cycle) is
begin
    if (cpu_on = '1' and rising_edge(cpu_ph1)) then
        if ((c_cycle(7 downto 1) & '0') = x"22") then
            cpu_run <= '1';
        end if;
    end if;
end process cpu_start;

vic_regs: process(cpu_on,cpu_ph2,c_cycle) is
begin
    if (cpu_on = '1' and rising_edge(cpu_ph2)) then
        case c_cycle is
            when x"00" =>
               vic_r1w0 <= '1';
               sid1_r1w0 <= '1';
               c_cycle <= c_cycle+1;
            when x"01" =>
                vic_r1w0 <= '1';
                c_cycle <= c_cycle+1;
            when x"02" =>
                vic_r1w0 <= '1';
                c_cycle <= c_cycle+1;
            when x"03" =>
                vic_r1w0 <= '1';
                c_cycle <= c_cycle+1;
            when x"04" =>
                vic_r1w0 <= '1';
                c_cycle <= c_cycle+1;
            when x"05" =>
                vic_r1w0 <= '1';
                c_cycle <= c_cycle+1;
            when x"06" =>
                vic_r1w0 <= '1';
                c_cycle <= c_cycle+1;
            when x"07" =>
                vic_r1w0 <= '1';
                c_cycle <= c_cycle+1;
            when x"08" =>
                vic_rga <= xtov6(x"18");
                vic_regw <= "00010100";
                vic_r1w0 <= '0';
                c_cycle <= c_cycle+1;
            when x"09" =>
                vic_rga <= xtov6(x"20");
                vic_regw <= x"0e";
                vic_r1w0 <= '0';
                c_cycle <= c_cycle+1;
            when x"0a" =>
                vic_rga <= xtov6(x"21");
                vic_regw <= x"06";
                vic_r1w0 <= '0';
                c_cycle <= c_cycle+1;
            when x"0b" =>
                vic_rga <= xtov6(x"11");
                vic_regw <= x"1b";
                --vic_regw <= x"17";
                vic_r1w0 <= '0';
                c_cycle <= c_cycle+1;
            when x"0c" =>
                vic_rga <= xtov6(x"16");
                vic_regw <= x"08";
                --vic_regw <= x"07";
                vic_r1w0 <= '0';
                c_cycle <= c_cycle+1;

            when x"0d" =>
                vic_rga <= xtov6(x"12");
                vic_r1w0 <= '1';
                sid1_rga <= xtov5(x"0e");
                sid1_dw <= x"25";
                sid1_r1w0 <= '0';
                c_cycle <= c_cycle+1;
            when x"0e" =>
                sid1_rga <= xtov5(x"0f");
                sid1_dw <= x"11";
                sid1_r1w0 <= '0';
                c_cycle <= c_cycle+1;
            when x"0f" =>
                sid1_rga <= xtov5(x"10");
                sid1_dw <= x"00";
                sid1_r1w0 <= '0';
                c_cycle <= c_cycle+1;
            when x"10" =>
                sid1_rga <= xtov5(x"11");
                sid1_dw <= x"08";
                sid1_r1w0 <= '0';
                c_cycle <= c_cycle+1;
            when x"11" =>
                sid1_rga <= xtov5(x"12");
                sid1_dw <= x"11";
                sid1_r1w0 <= '0';
                c_cycle <= c_cycle+1;
            when x"12" =>
                sid1_rga <= xtov5(x"13");
                sid1_dw <= x"0f";
                sid1_r1w0 <= '0';
                c_cycle <= c_cycle+1;
            when x"13" =>
                sid1_rga <= xtov5(x"14");
                sid1_dw <= x"ff";
                sid1_r1w0 <= '0';
                c_cycle <= c_cycle+1;

            when x"14" =>
                sid1_rga <= xtov5(x"07");
                sid1_dw <= x"9a";
                sid1_r1w0 <= '0';
                c_cycle <= c_cycle+1;
            when x"15" =>
                sid1_rga <= xtov5(x"08");
                sid1_dw <= x"15";
                sid1_r1w0 <= '0';
                c_cycle <= c_cycle+1;
            when x"16" =>
                sid1_rga <= xtov5(x"09");
                sid1_dw <= x"00";
                sid1_r1w0 <= '0';
                c_cycle <= c_cycle+1;
            when x"17" =>
                sid1_rga <= xtov5(x"0a");
                sid1_dw <= x"08";
                sid1_r1w0 <= '0';
                c_cycle <= c_cycle+1;
            when x"18" =>
                sid1_rga <= xtov5(x"0b");
                sid1_dw <= x"11";
                sid1_r1w0 <= '0';
                c_cycle <= c_cycle+1;
            when x"19" =>
                sid1_rga <= xtov5(x"0c");
                sid1_dw <= x"0f";
                sid1_r1w0 <= '0';
                c_cycle <= c_cycle+1;
            when x"1a" =>
                sid1_rga <= xtov5(x"0d");
                sid1_dw <= x"ff";
                sid1_r1w0 <= '0';
                c_cycle <= c_cycle+1;

            when x"1b" =>
                sid1_rga <= xtov5(x"00");
                sid1_dw <= x"81";
                sid1_r1w0 <= '0';
                c_cycle <= c_cycle+1;
            when x"1c" =>
                sid1_rga <= xtov5(x"01");
                sid1_dw <= x"19";
                sid1_r1w0 <= '0';
                c_cycle <= c_cycle+1;
            when x"1d" =>
                sid1_rga <= xtov5(x"02");
                sid1_dw <= x"00";
                sid1_r1w0 <= '0';
                c_cycle <= c_cycle+1;
            when x"1e" =>
                sid1_rga <= xtov5(x"03");
                sid1_dw <= x"08";
                sid1_r1w0 <= '0';
                c_cycle <= c_cycle+1;
            when x"1f" =>
                sid1_rga <= xtov5(x"04");
                sid1_dw <= x"11";
                sid1_r1w0 <= '0';
                c_cycle <= c_cycle+1;
            when x"20" =>
                sid1_rga <= xtov5(x"05");
                sid1_dw <= x"0f";
                sid1_r1w0 <= '0';
                c_cycle <= c_cycle+1;
            when x"21" =>
                sid1_rga <= xtov5(x"06");
                sid1_dw <= x"ff";
                sid1_r1w0 <= '0';
                c_cycle <= c_cycle+1;


            when x"22" =>
                sid1_rga <= xtov5(x"18");
                sid1_dw <= x"0f";
                sid1_r1w0 <= '0';
                c_cycle <= c_cycle+1;
            when others =>
                vic_rga <= xtov6(x"12");
                vic_r1w0 <= '1';
                sid1_rga <= xtov5(x"12");
                sid1_dw <= sw & x"1";
                sid1_r1w0 <= '0';
        end case;
    end if;
end process vic_regs;

sid_1: sid6581 port map (
    res0        => res0,
    ph2         => cpu_ph2,
    rga         => sid1_rga,
    din         => sid1_dw,
    dout        => sid1_dr,
    r1w0        => sid1_r1w0,
    s16audio    => sid1_out
);

ac_muten <= res0;
ac_mclk <= clk12;

bclk_gen : process(clk12,bclk_cnt,res1) is
variable inc_0 : std_logic;
variable inc_1 : std_logic;
begin
    inc_0 := not bclk_cnt(0);
    inc_1 := bclk_cnt(1) xor bclk_cnt(0);
    if (res1 = '1') then
        bclk_cnt <= "00";
    elsif (rising_edge(clk12)) then
        bclk_cnt <= inc_1 & inc_0;
    end if;
end process bclk_gen;

ac_bclk <= bclk_cnt(1);

audio_send : process(bclk_cnt,res1,sid1_out,hold_srl,audio_frame) is
variable stage : word;
variable frmZ : std_logic;
begin
    if isZero6(audio_frame) then
        stage := word(sid1_out);
        frmZ := '1';
    else
        -- srl left-rotates to send MSB first
        stage := hold_srl(14 downto 0) & hold_srl(15);
        frmZ := '0';
    end if;
    if (res1 = '1') then
        audio_frame <= "000000";
    elsif (falling_edge(bclk_cnt(1))) then
        if (audio_frame(4) = '0') then
          -- MSB first
          ac_pbdat <= stage(15);
        else
            -- I get a corrupt right channel if I try to
            -- duplciate the data going to the left
            ac_pbdat <= '0';
        end if;
        ac_pblrc <= audio_frame(4);
        ac_pblrc <= frmZ;
        hold_srl <= stage;
        audio_frame <= adv_frame(audio_frame);
    end if;
end process audio_send;

--
-- I2C config for SSM2603
--
i2c: component i2c_xcvr port map (
    clk1M       => cpu_ph2,
    res0        => res0,
    init_line   => ssm_init_line,
    init_data   => ssm_init_data,
    error       => ssm_error,
    scl         => ac_scl,
    sda         => ac_sda
);
ssm_init_data <= dac_init(ssm_init_line);
led(0) <= ssm_error;

end testing;
