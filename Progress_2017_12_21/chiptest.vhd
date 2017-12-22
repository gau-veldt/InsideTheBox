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
    -- for keyboard
    je          :   inout   std_logic_vector(7 downto 0);

    vga_hs      :   out     std_logic;
    vga_vs      :   out     std_logic;
    vga_r       :   out     std_logic_vector(4 downto 0);
    vga_g       :   out     std_logic_vector(5 downto 0);
    vga_b       :   out     std_logic_vector(4 downto 0)
);
end chip_test;

architecture testing of chip_test is

constant use_init_fsm : boolean := false;

attribute mark_debug    : string;
attribute keep          : string;

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
subtype u4 is unsigned(3 downto 0);
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
subtype slv11 is std_logic_vector(10 downto 0);
subtype u12 is unsigned(11 downto 0);
subtype slv14 is std_logic_vector(13 downto 0);
subtype word is std_logic_vector(15 downto 0);
subtype u16 is unsigned(15 downto 0);
subtype s16 is signed(15 downto 0);
subtype slv17 is std_logic_vector(16 downto 0);
subtype slv20 is std_logic_vector(19 downto 0);
subtype slv24 is std_logic_vector(23 downto 0);
subtype slv27 is std_logic_vector(26 downto 0);     -- I2C SSM2603 send with ACK pads

signal res0         : std_logic;
signal res1         : std_logic;
signal c12res1      : std_logic;                        -- 12 MHz domain reset
signal c12fdr1      : std_logic;

signal cpu_r1w0     : std_logic:='1';
signal cpu_r0w1     : std_logic;
signal cpu_run      : std_logic:='0';
signal cpu_rclk     : std_logic;
signal cpu_in_cycle : std_logic;

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
constant init_wait_to   : u16 := x"0014";
signal init_counter     : u16 := x"0000";
signal init_cnt_less1   : u16 := x"ffff";
signal init_addr        : word;
signal init_data        : byte;
signal init_r1w0        : std_logic := '1';
signal init_r0w1        : std_logic;
signal rd_init          : slv24 :=  x"000000";              -- output of init ROM
function ram_init(lin: u16) return slv24 is
begin
    -- values for init ROM
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
        when x"0037"    => return x"dbe7" & x"0e";--
        when x"0038"    => return x"8000" & x"1e";
        when x"0039"    => return x"8001" & x"80";
        when x"003a"    => return x"8002" & x"5e";
        when x"003b"    => return x"8003" & x"fe";
        when x"003c"    => return x"8004" & x"c3";
        when x"003d"    => return x"8005" & x"ff";--"c2";
        when x"003e"    => return x"8006" & x"cd";
        when x"003f"    => return x"8007" & x"38";
        when x"0040"    => return x"8008" & x"30";
        when x"0041"    => return x"8009" & x"81";
        when x"0042"    => return x"800a" & x"19";
        when x"0043"    => return x"800b" & x"00";
        when x"0044"    => return x"800c" & x"08";
        when x"0045"    => return x"800d" & x"11";
        when x"0046"    => return x"800e" & x"0f";
        when x"0047"    => return x"800f" & x"ff";
        when x"0048"    => return x"8010" & x"9a";
        when x"0049"    => return x"8011" & x"15";
        when x"004a"    => return x"8012" & x"00";
        when x"004b"    => return x"8013" & x"08";
        when x"004c"    => return x"8014" & x"11";
        when x"004d"    => return x"8015" & x"0f";
        when x"004e"    => return x"8016" & x"ff";
        when x"004f"    => return x"8017" & x"25";
        when x"0050"    => return x"8018" & x"11";
        when x"0051"    => return x"8019" & x"00";
        when x"0052"    => return x"801a" & x"08";
        when x"0053"    => return x"801b" & x"11";
        when x"0054"    => return x"801c" & x"0f";
        when x"0055"    => return x"801d" & x"ff";
        when x"0056"    => return x"801e" & x"a2";  --  LDX
        when x"0057"    => return x"801f" & x"15";  --      #$15
        when x"0058"    => return x"8020" & x"bd";  --  LDA
        when x"0059"    => return x"8021" & x"08";  --
        when x"005a"    => return x"8022" & x"80";  --      $8008,X
        when x"005b"    => return x"8023" & x"9d";  --  STA
        when x"005c"    => return x"8024" & x"ff";  --
        when x"005d"    => return x"8025" & x"d3";  --      $D3FF,X
        when x"005e"    => return x"8026" & x"ca";  --  DEX
        when x"005f"    => return x"8027" & x"d0";  --  BNE
        when x"0060"    => return x"8028" & x"f7";  --      $8020
        when x"0061"    => return x"8029" & x"a9";  --  LDA
        when x"0062"    => return x"802a" & x"0f";  --      #$0F
        when x"0063"    => return x"802b" & x"8d";  --  STA
        when x"0064"    => return x"802c" & x"18";  --
        when x"0065"    => return x"802d" & x"d4";  --      $D418
        when x"0066"    => return x"802e" & x"ee";  --  INC
        when x"0067"    => return x"802f" & x"08";  --
        when x"0068"    => return x"8030" & x"80";  --      $8008
        when x"0069"    => return x"8031" & x"4c";  --  JMP
        when x"006a"    => return x"8032" & x"ef";  --
        when x"006b"    => return x"8033" & x"fc";  --      $FCEF
        when others     => return x"0000" & x"00";      -- 0 (and 1) inaccessible on C64
    end case;
end ram_init; 
constant ram_init_count : u16 := x"006c";

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

signal reset_wait : u12 := x"000";
constant reset_delay : u12 := x"0fb";       -- number of 125 MHz clocks in 2000 ns

signal cgrom_data : byte;
signal lorom_data : byte;
signal hirom_data : byte;

component cia6526 is
port (
    PAo             : out   std_logic_vector(7 downto 0);
    PAi             : in    std_logic_vector(7 downto 0);
    PBo             : out   std_logic_vector(7 downto 0);
    PBi             : in    std_logic_vector(7 downto 0);
    irq0            : out   std_logic;
    
    rga             : in    std_logic_vector(3 downto 0);
    rgdi            : in    std_logic_vector(7 downto 0);    
    rgdo            : out   std_logic_vector(7 downto 0);
    r1w0            : in    std_logic;
    ce              : in    std_logic;
    clk             : in    std_logic                               -- 1 MHz    
);
end component;

signal cia1EN       : std_logic;
signal cia2EN       : std_logic;
signal cia1PAi      : byte;
signal cia1PAo      : byte;
signal cia1PBi      : byte;
signal cia1PBo      : byte;
signal cia1_irq     : std_logic;
signal cia1_rga     : nybble;
signal cia1_dr      : byte;
signal cia1_dw      : byte;
signal cia1_r1w0    : std_logic;

signal cia2PAi      : byte;
signal cia2PAo      : byte;
signal cia2PBi      : byte;
signal cia2PBo      : byte;
signal cia2_irq     : std_logic;
signal cia2_rga     : nybble;
signal cia2_dr      : byte;
signal cia2_dw      : byte;
signal cia2_r1w0    : std_logic;

signal irq_tie      : std_logic;

component ROM_IEC
port (
    ATN0        :   in  std_logic;          -- master broadcasting command when low
    LCLK0       :   in  std_logic;          -- input clock when listening 0=low=idle 1=bit valid
    DATAI0      :   in  std_logic;          -- input data when listening
    TCLK0       :   out std_logic;          -- output clock when talking  0=low=idle 1=bit valid
    DATAO0      :   out std_logic;          -- output data when talking

    clk         : in std_logic              -- system clock (use PH2)
);
end component;
signal  iecMATN0    : std_logic := '1';
signal  iecMCLK0    : std_logic := '1';
signal  iecMDATA0   : std_logic := '1';
signal  iecSCLK0    : std_logic;
signal  iecSDATA0   : std_logic;

--
--  For accessing PS/2 keyboard (via PMOD-2-PS/2 accessory)
--
signal kbled_s      : slv3 := "000";        -- slave  for kdb leds
signal kbled        : slv3 := "000";        -- master for kbd leds
signal kbled_ne     : std_logic;            -- dirty. master!=slave
alias kb_data : std_logic is je(0);         -- PS/2 data  io
alias kb_clk  : std_logic is je(2);         -- PS/2 clock io
signal kb_din       : std_logic;            -- PS/2 data  i
signal kb_clkin     : std_logic;            -- PS/2 clock i
signal kb_dout      : std_logic := '1';     -- PS/2 data  o
signal kb_clkout    : std_logic := '1';     -- PS/2 clock o
--signal kb_clkout    : std_logic := '0';     -- PS/2 clock o (NB: keep the 0 here!)
signal kb_slow      : u6 := "000000";       -- counter to yield 64 usec period from 1 MHz
alias kb_fsm_clk : std_logic is kb_slow(5); -- msb of counter works as clock
signal kbi_sr   : slv11 := "00000000000";
signal kbi_cnt  : u4 := x"0";

type kb_ofsm_mode is (
    kbo_idle
);
signal kb_ofsm : kb_ofsm_mode := kbo_idle;
signal kbo_sr   : slv11 := "00000000000";
signal kbo_p    : std_logic;                -- to hold parity calc

type kbsel_t is array(7 downto 0) of byte;
signal kbsel : kbsel_t := (others => x"FF");
alias kbsel0 : byte is kbsel(0);
alias kbsel1 : byte is kbsel(1);
alias kbsel2 : byte is kbsel(2);
alias kbsel3 : byte is kbsel(3);
alias kbsel4 : byte is kbsel(4);
alias kbsel5 : byte is kbsel(5);
alias kbsel6 : byte is kbsel(6);
alias kbsel7 : byte is kbsel(7);
signal kb_restore : std_logic := '1';

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

reset_12M_bridge: process(clk12,res1) is
begin
    if (res1='1') then
        c12fdr1 <= '1';
        c12res1 <= '1';
    elsif rising_edge(clk12) then
        c12fdr1 <= '0';
        c12res1 <= c12fdr1;
    end if;
end process reset_12M_bridge;

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

irq_tie <= cia1_irq;
--irq_tie <= not btn(3);
led(3) <= not irq_tie;

c6510_irq0 <= irq_tie;
c6510_nmi0 <= '1';
c6510_so0 <= '1';
c6510_rdy <= '1';
cpu_rclk <= cpu_ph4x and cpu_run;

cpu_buscycle: process(cpu_on, cpu_ph1) is
begin
    if (falling_edge(cpu_on)) then
        cpu_in_cycle <= '0';
    end if;
    if ((cpu_on='1') and rising_edge(cpu_ph1)) then
        cpu_in_cycle <= '1';
    end if;
end process cpu_buscycle;

init_cnt_less1 <= init_counter -1;
initrom_latch: process(init_cnt_less1,clk160) is
begin
    if (rising_edge(clk160)) then
        rd_init <= ram_init(init_cnt_less1);
    end if;
end process initrom_latch;
init_addr <= rd_init(23 downto 8);
init_data <= rd_init(7 downto 0);

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
                        --init_addr <= x"ffff";
                        --init_data <= x"ff";
                    else
                        init_counter <= init_counter + 1;
                    end if;
                when initram_xfer =>
                    if (init_counter = ram_init_count) then
                        init_counter <= x"0000";
                        init_stg <= initram_idle;
                    else
                        --init_addr <= ram_init(init_counter)(23 downto 8);
                        --init_data <= ram_init(init_counter)(7 downto 0);
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
--  TODO: mbk_xio2,mbk_xio1
with cur_a_bank select cpudi <=
    ramdr                           when mbk_ram,
    ramdr(7 downto 4) & cramdr      when mbk_cram,
    vic_regr                        when mbk_vic,
    sid1_dr                         when mbk_sid,
    cia1_dr                         when mbk_cia1,
    cia2_dr                         when mbk_cia2,
    cgrom_data                      when mbk_cgrom,
    lorom_data                      when mbk_lorom,
    hirom_data                      when mbk_hirom,
    ramdr                           when others;

with cur_a_bank select cia1EN <= '1' when mbk_cia1, '0' when others;
with cur_a_bank select cia2EN <= '1' when mbk_cia2, '0' when others;

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


cpu_vicsid: if not use_init_fsm generate

cpu_start: process(cpu_on,cpu_ph1,init_stg) is
begin
    if (cpu_on = '1' and init_stg=initram_idle and rising_edge(cpu_ph1)) then
        cpu_run <= '1';
    end if;
end process cpu_start;

vic_rga     <= abus(5 downto 0);
sid1_rga    <= abus(4 downto 0);
cia1_rga    <= abus(3 downto 0);
cia2_rga    <= abus(3 downto 0);
vic_regw    <= cpudo;
sid1_dw     <= cpudo;
cia1_dw     <= cpudo;
cia2_dw     <= cpudo;

process(cpu_in_cycle,cpu_r1w0,cur_a_bank) is
--
--  chip write select logic
--
begin
    if (cpu_in_cycle='1') then
        if (cur_a_bank = mbk_vic) then
            vic_r1w0 <= cpu_r1w0;
        else
            vic_r1w0 <= '1';
        end if;
        if (cur_a_bank = mbk_sid) then
            sid1_r1w0 <= cpu_r1w0;
        else
            sid1_r1w0 <= '1';
        end if;
        if (cur_a_bank = mbk_cia1) then
            cia1_r1w0 <= cpu_r1w0;
        else
            cia1_r1w0 <= '1';
        end if;
        if (cur_a_bank = mbk_cia2) then
            cia2_r1w0 <= cpu_r1w0;
        else
            cia2_r1w0 <= '1';
        end if;
    end if;
end process;

end generate;

fsm_vicsid: if use_init_fsm generate

cpu_start: process(cpu_on,cpu_ph1,c_cycle) is
begin
    if (cpu_on = '1' and rising_edge(cpu_ph1)) then
        if ((c_cycle(7 downto 1) & '0') = x"22") then
            cpu_run <= '1';
        end if;
    end if;
end process cpu_start;

process(cpu_on,cpu_ph2,c_cycle) is
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
end generate;

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
    if (c12res1 = '1') then
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
    if (c12res1 = '1') then
        audio_frame <= "000000";
    elsif (falling_edge(bclk_cnt(1))) then
        if (audio_frame(4) = '0') then
          -- MSB first
          ac_pbdat <= stage(15);
        else
            -- I get a corrupt right channel if I try to
            -- duplciate the data going to the left
            --ac_pbdat <= stage(15);
            ac_pbdat <= '0';
        end if;
        --ac_pblrc <= audio_frame(4);
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
--led(0) <= ssm_error;

cia1: component cia6526 port map (
    clk         => cpu_ph2,
    ce          => cia1EN,
    rga         => cia1_rga,
    rgdi        => cia1_dw,
    rgdo        => cia1_dr,
    r1w0        => cia1_r1w0,
    irq0        => cia1_irq,
    PAi         => cia1PAi,
    PAo         => cia1PAo,
    PBi         => cia1PBi,
    PBo         => cia1PBo
);

cia2: component cia6526 port map (
    clk         => cpu_ph2,
    ce          => cia2EN,
    rga         => cia2_rga,
    rgdi        => cia2_dw,
    rgdo        => cia2_dr,
    r1w0        => cia2_r1w0,
    irq0        => cia2_irq,
    PAi         => cia2PAi,
    PAo         => cia2PAo,
    PBi         => cia2PBi,
    PBo         => cia2PBo
);

iecrom: ROM_IEC port map (
    ATN0        =>  iecMATN0,
    LCLK0       =>  iecMCLK0,
    DATAI0      =>  iecMDATA0,

    TCLK0       =>  iecSCLK0,
    DATAO0      =>  iecSDATA0,

    clk         =>  cpu_ph2
);

iecMDATA0 <= not cia2PAo(5);
iecMCLK0  <= not cia2PAo(4);
iecMATN0  <= not cia2PAo(3);
cia2PAi <= iecSDATA0 & iecSCLK0 & "111111";
cia2PBi <= "11111111";

--
--  PS/2 keyboard driver
--
--  NB: je(0),je(2) choice is partly based on my hardware:
--      a PMOD12-to-2xPMOD6 splitter connected at PMOD JE where each PMOD6
--      connects to a digilent PS/2-to-PMOD6 accessory (one for keys, one
--      for mouse).  JE is the only "standard" (series protection, unpaired
--      signals) PMOD on the Zybo.  The PS/2 accessor does not need the
--      high speed differentials of the high-speed PMODs and using JF (MIO
--      which is PS-side) renderes the connected PMOD(s) unavailable from
--      the PL (FPGA) side.
--
--      JE(0) is the PS/2 data line.
--      JE(2) is the PS/2 clock line.
--
--signal kbled_s      : slv3;                 -- slave  for kdb leds
--signal kbled        : slv3;                 -- master for kbd leds
--signal kbled_ne     : std_logic;            -- dirty. master!=slave
--

--  first let's obtain the input (real) state of the lines
kb_din      <= kb_data;
kb_clkin    <= kb_clk;

--  now the driving logic
with kb_dout select kb_data <=
    '0' when '0',                   -- drive the 0
    'Z' when others;                -- high-Z == don't drive == line pulled to 1
with kb_clkout select kb_clk <=
    '0' when '0',                   -- drive the 0
    'Z' when others;                -- high-Z == don't drive == line pulled to 1

--  the handshake...
--      The PS/2 port will only send data when the clock is released.
--      If we drive the clock low no data will be sent.  When we wish to
--      send data we follow the same rule and never send when we find
--      the clock low. Normally we will find this line high when we wish
--      to send since any time it isn't our FSM is going to be in a receive
--      frame and thus not trying to send.
--
--      An input-only FSM can be driven just off the PS/2's clock signal.
--
--      To send we can have this machine pause whenever the dirty bit flips up
--      indicating the status of keyboard leds should change.  It then pauses
--      once outside a frame and resumes once we clear the dirty bit in the
--      sending FSM.  The sending FSM needs its own clock since the PS/2 port
--      only clocks its own transmission.  The slowest ones around are the
--      PH1/PH2 clocks used to clock the 6510 and they are too fast since a
--      1 usec period is too fast since the minimum edge period for PS/2 port
--      is 30 so we need a slowdown for clocking the sending FSM.  32 usec
--      edges will work fine (max is 60) so we'll use a 6-bit counter's msb as
--      the clock which transitions every 32 usec if the counter counts on the
--      1 MHz PH2 clock.

--kbled_ne <= (kbled(2) xor kbled_s(2) ) or
--            (kbled(1) xor kbled_s(1) ) or
--            (kbled(0) xor kbled_s(0) );
--ps2_slow: process(cpu_ph2,kb_slow) is
--begin
--    if (rising_edge(cpu_ph2)) then
--        kb_slow <= kb_slow + 1;
--    end if;
--end process ps2_slow;

ps2_recv: process(kb_clkin,kb_din) is
variable curCnt : u4;
variable data : slv11;
variable code : byte;
variable ext : std_logic := '0';
variable iext : std_logic := '1';
variable brk : std_logic := '0';
variable rkey,xkey : std_logic;
variable restore_key : std_logic := '1';
begin
    led(1) <= not kb_clkin;
    led(0) <= not kb_din;
    if (falling_edge(kb_clkin)) then
        curCnt := kbi_cnt;
        data := kbi_sr;
        curCnt := curCnt + 1;
        data := kb_din & kbi_sr(10 downto 1);
        if (curCnt = x"b") then
            curCnt := x"0";
            code := data(8 downto 1);
            if (code = x"E0") then
                ext := '1';
                iext := '0';
            elsif (code = x"F0") then
                brk := '1';
            else
                rkey := brk or ext;
                xkey := brk or iext;
                case code is                                                -- PC   (C64)
                    when x"71" => kbsel0(0) <= xkey;                        -- DEL
                    when x"70" => kbsel0(0) <= xkey; kbsel1(7) <= xkey;     -- INS (shift+DEL)
                    when x"5A" => kbsel0(1) <= rkey;                        -- ENTER (RETURN)
                    when x"74" => kbsel0(2) <= xkey;                        -- right
                    when x"6B" => kbsel0(2) <= xkey; kbsel1(7) <= xkey;     -- left (shift+CRSR RT)
                    when x"83" => kbsel0(3) <= rkey;                        -- F7
                    when x"0A" => kbsel0(3) <= rkey; kbsel1(7) <= rkey;     -- F8 (shift+F7)
                    when x"05" => kbsel0(4) <= rkey;                        -- F1
                    when x"06" => kbsel0(4) <= rkey; kbsel1(7) <= rkey;     -- F2 (shift+F1)
                    when x"04" => kbsel0(5) <= rkey;                        -- F3
                    when x"0c" => kbsel0(5) <= rkey; kbsel1(7) <= rkey;     -- F4 (shift+F3)
                    when x"03" => kbsel0(6) <= rkey;                        -- F5
                    when x"0B" => kbsel0(6) <= rkey; kbsel1(7) <= rkey;     -- F6 (shift+F5)
                    when x"72" => kbsel0(7) <= xkey;                        -- dowm
                    when x"75" => kbsel0(7) <= xkey; kbsel1(7) <= xkey;     -- up (shift+CRSR DN)
                                  kbsel6(6) <= rkey;                        -- KP 8 (exp arrow)
                    when x"26" => kbsel1(0) <= rkey;                        -- 3
                    when x"1D" => kbsel1(1) <= rkey;                        -- W
                    when x"1C" => kbsel1(2) <= rkey;                        -- A
                    when x"25" => kbsel1(3) <= rkey;                        -- 4
                    when x"1A" => kbsel1(4) <= rkey;                        -- Z
                    when x"1B" => kbsel1(5) <= rkey;                        -- S
                    when x"24" => kbsel1(6) <= rkey;                        -- E
                    when x"12" => kbsel1(7) <= rkey;                        -- LSHIFT
                    when x"2E" => kbsel2(0) <= rkey;                        -- 5
                    when x"2D" => kbsel2(1) <= rkey;                        -- R
                    when x"23" => kbsel2(2) <= rkey;                        -- D
                    when x"36" => kbsel2(3) <= rkey;                        -- 6
                    when x"21" => kbsel2(4) <= rkey;                        -- C
                    when x"2B" => kbsel2(5) <= rkey;                        -- F
                    when x"2C" => kbsel2(6) <= rkey;                        -- T
                    when x"22" => kbsel2(7) <= rkey;                        -- X
                    when x"3D" => kbsel3(0) <= rkey;                        -- 7
                    when x"35" => kbsel3(1) <= rkey;                        -- Y
                    when x"34" => kbsel3(2) <= rkey;                        -- G
                    when x"3E" => kbsel3(3) <= rkey;                        -- 8
                    when x"32" => kbsel3(4) <= rkey;                        -- B
                    when x"33" => kbsel3(5) <= rkey;                        -- H
                    when x"3C" => kbsel3(6) <= rkey;                        -- U
                    when x"2A" => kbsel3(7) <= rkey;                        -- V
                    when x"46" => kbsel4(0) <= rkey;                        -- 9
                    when x"43" => kbsel4(1) <= rkey;                        -- I
                    when x"3B" => kbsel4(2) <= rkey;                        -- J
                    when x"45" => kbsel4(3) <= rkey;                        -- 0
                    when x"3A" => kbsel4(4) <= rkey;                        -- M
                    when x"42" => kbsel4(5) <= rkey;                        -- K
                    when x"44" => kbsel4(6) <= rkey;                        -- O
                    when x"31" => kbsel4(7) <= rkey;                        -- N
                    when x"79" => kbsel5(0) <= rkey;                        -- KP + (+)
                    when x"4D" => kbsel5(1) <= rkey;                        -- P
                    when x"4B" => kbsel5(2) <= rkey;                        -- L
                    when x"7B" => kbsel5(3) <= rkey;                        -- KP - (-)
                    when x"4E" => kbsel5(3) <= rkey;                        -- -
                    when x"49" => kbsel5(4) <= rkey;                        -- .
                    when x"4C" => kbsel5(5) <= rkey;                        -- ; (:)
                    when x"54" => kbsel5(6) <= rkey;                        -- [ (@)
                    when x"41" => kbsel5(7) <= rkey;                        -- ,
                    when x"5D" => kbsel6(0) <= rkey;                        -- \ (GBP)
                    when x"5B" => kbsel6(1) <= rkey;                        -- ] (*)
                    when x"52" => kbsel6(2) <= rkey;                        -- ' (;)
                    when x"6C" => kbsel6(3) <= xkey;                        -- HOME (HOME)
                    when x"59" => kbsel6(4) <= rkey;                        -- RSHIFT
                    when x"55" => kbsel6(5) <= rkey;                        -- =
--                  when x"75" *** SEE LINE 1619 ***                        -- KP 8 (exp arrow)
                    when x"4A" => kbsel6(7) <= rkey;                        -- /
                    when x"16" => kbsel7(0) <= rkey;                        -- 1
                    when x"66" => kbsel7(1) <= rkey;                        -- BS ( <-- )
                    when x"0D" => kbsel7(2) <= rkey;                        -- TAB (CTRL)
                    when x"1E" => kbsel7(3) <= rkey;                        -- 2
                    when x"29" => kbsel7(4) <= rkey;                        -- SPACE
                    when x"1F" => kbsel7(5) <= xkey;                        -- WINDOZE (C=)
                    when x"15" => kbsel7(6) <= rkey;                        -- Q
                    when x"76" => kbsel7(7) <= rkey;                        -- ESC (STOP)
                    when x"2F" => restore_key := xkey;                      -- MENU (RESTORE)
                    when others =>
                        null;
                end case;
                brk := '0';
                ext := '0';
                iext := '1';
            end if;
        end if;
    end if;
    kbi_sr <= data;
    kbi_cnt <= curCnt;
    kb_restore <= restore_key;
end process ps2_recv;

kbport: process(cia1PAo,kbsel) is
variable matrix : byte;
begin
    matrix := x"FF";
    for sel in 7 downto 0 loop
        if (cia1PAo(sel)='0') then
            matrix := matrix and kbsel(sel);
        end if;
    end loop;
    cia1PBi <= matrix;
end process kbport;

end testing;
