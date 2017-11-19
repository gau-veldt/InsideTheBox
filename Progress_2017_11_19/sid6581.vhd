----------------------------------------------------------------------------------
--
--  SID chip (audio)
-- 
----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity sid6581 is
port (
    res0        :   in  std_logic;
    ph2         :   in  std_logic;
    rga         :   in  std_logic_vector(4 downto 0);
    din         :   in  std_logic_vector(7 downto 0);
    dout        :   out std_logic_vector(7 downto 0);
    r1w0        :   in  std_logic;
    s16audio    :   out signed(15 downto 0)
);
end sid6581;

architecture sid_impl of sid6581 is

signal r0w1         :   std_logic;

attribute gated_clock : string;
--attribute gated_clock of ph2:signal is "true";

component sid_voice is
port (
    clk1M       : in    std_logic;
    freq        : in    unsigned            (15 downto 0);
    pw          : in    unsigned            (11 downto 0);
    wvfm        : in    std_logic_vector    (3 downto 0);
    test        : in    std_logic;
    rmod        : in    std_logic;
    sync        : in    std_logic;
    gate        : in    std_logic;
    enva        : in    unsigned            (3 downto 0);
    envd        : in    unsigned            (3 downto 0);
    envs        : in    unsigned            (3 downto 0);
    envr        : in    unsigned            (3 downto 0);
    envo        : out   unsigned            (7 downto 0);
    osco        : out   unsigned            (7 downto 0);
    uout        : out   unsigned            (7 downto 0)
);
end component;

component ccm_85
port (
    clk         :  in STD_LOGIC;
    a           :  in STD_LOGIC_VECTOR(9 DOWNTO 0);
    p           : out STD_LOGIC_VECTOR(15 DOWNTO 0)
);
end component;

subtype u3      is unsigned             (2 downto 0);
subtype u4      is unsigned             (3 downto 0);
subtype nybble  is std_logic_vector     (3 downto 0);
subtype u8      is unsigned             (7 downto 0);
subtype byte    is std_logic_vector     (7 downto 0);
subtype slv10   is std_logic_vector     (9 downto 0);
subtype s10     is signed               (9 downto 0);
subtype u11     is unsigned             (10 downto 0);
subtype u12     is unsigned             (11 downto 0);
subtype slv12   is std_logic_vector     (11 downto 0);
subtype s16     is signed               (15 downto 0);
subtype u16     is unsigned             (15 downto 0);
subtype word    is std_logic_vector     (15 downto 0);
subtype s20     is signed               (19 downto 0);

signal clk1m        :   std_logic;

type sidregs_t is array(28 downto 0) of byte;
signal sid_regs : sidregs_t := (others => x"00");

signal  v1_freq     :   u16;
signal  v1_pw       :   u12;
alias   v1_wvfm     :   nybble      is      sid_regs( 4)(7 downto 4);  
alias   v1_test     :   std_logic   is      sid_regs( 4)(3);  
alias   v1_ring     :   std_logic   is      sid_regs( 4)(2);  
alias   v1_sync     :   std_logic   is      sid_regs( 4)(1);  
alias   v1_gate     :   std_logic   is      sid_regs( 4)(0);  
alias   v1_enva     :   nybble      is      sid_regs( 5)(7 downto 4);
alias   v1_envd     :   nybble      is      sid_regs( 5)(3 downto 0);
alias   v1_envs     :   nybble      is      sid_regs( 6)(7 downto 4);
alias   v1_envr     :   nybble      is      sid_regs( 6)(3 downto 0);
signal  v1_envo     :   u8;
signal  v1_osco     :   u8;
signal  v1_out      :   u8;

signal  v2_freq     :   u16;
signal  v2_pw       :   u12;
alias   v2_wvfm     :   nybble      is         sid_regs(11)(7 downto 4);  
alias   v2_test     :   std_logic   is         sid_regs(11)(3);  
alias   v2_ring     :   std_logic   is         sid_regs(11)(2);  
alias   v2_sync     :   std_logic   is         sid_regs(11)(1);  
alias   v2_gate     :   std_logic   is         sid_regs(11)(0);  
alias   v2_enva     :   nybble      is      sid_regs(12)(7 downto 4);
alias   v2_envd     :   nybble      is      sid_regs(12)(3 downto 0);
alias   v2_envs     :   nybble      is      sid_regs(13)(7 downto 4);
alias   v2_envr     :   nybble      is      sid_regs(13)(3 downto 0);
signal  v2_envo     :   u8;
signal  v2_osco     :   u8;
signal  v2_out      :   u8;

signal  v3_freq     :   u16;
signal  v3_pw       :   u12;
alias   v3_wvfm     :   nybble      is      sid_regs(18)(7 downto 4);  
alias   v3_test     :   std_logic   is      sid_regs(18)(3);  
alias   v3_ring     :   std_logic   is      sid_regs(18)(2);  
alias   v3_sync     :   std_logic   is      sid_regs(18)(1);  
alias   v3_gate     :   std_logic   is      sid_regs(18)(0);  
alias   v3_enva     :   nybble      is      sid_regs(19)(7 downto 4);
alias   v3_envd     :   nybble      is      sid_regs(19)(3 downto 0);
alias   v3_envs     :   nybble      is      sid_regs(20)(7 downto 4);
alias   v3_envr     :   nybble      is      sid_regs(20)(3 downto 0);
signal  v3_envo     :   u8;
signal  v3_osco     :   u8;
signal  v3_out      :   u8;

signal  filter_fc   :   u11;
alias   filter_res  :   u4          is      u4(sid_regs(23)(7 downto 4));
alias   filter_v3   :   std_logic   is      sid_regs(23)(2);
alias   filter_v2   :   std_logic   is      sid_regs(23)(1);
alias   filter_v1   :   std_logic   is      sid_regs(23)(0);

alias   mute3       :   std_logic   is      sid_regs(24)(7);
alias   filter_hp   :   std_logic   is      sid_regs(24)(6);
alias   filter_bp   :   std_logic   is      sid_regs(24)(5);
alias   filter_lp   :   std_logic   is      sid_regs(24)(4);
alias   volume      :   nybble      is      sid_regs(24)(3 downto 0);

alias   pot_x       :   byte        is      sid_regs(25);
alias   pot_y       :   byte        is      sid_regs(26);
alias   osc3rnd     :   byte        is      sid_regs(27);
alias   env3        :   byte        is      sid_regs(28);

function u8tos10(uns : u8) return s10 is
begin
    case uns is
        when x"00"  => return to_signed(-128,10);
        when x"01"  => return to_signed(-127,10);
        when x"02"  => return to_signed(-126,10);
        when x"03"  => return to_signed(-125,10);
        when x"04"  => return to_signed(-124,10);
        when x"05"  => return to_signed(-123,10);
        when x"06"  => return to_signed(-122,10);
        when x"07"  => return to_signed(-121,10);
        when x"08"  => return to_signed(-120,10);
        when x"09"  => return to_signed(-119,10);
        when x"0a"  => return to_signed(-118,10);
        when x"0b"  => return to_signed(-117,10);
        when x"0c"  => return to_signed(-116,10);
        when x"0d"  => return to_signed(-115,10);
        when x"0e"  => return to_signed(-114,10);
        when x"0f"  => return to_signed(-113,10);
        when x"10"  => return to_signed(-112,10);
        when x"11"  => return to_signed(-111,10);
        when x"12"  => return to_signed(-110,10);
        when x"13"  => return to_signed(-109,10);
        when x"14"  => return to_signed(-108,10);
        when x"15"  => return to_signed(-107,10);
        when x"16"  => return to_signed(-106,10);
        when x"17"  => return to_signed(-105,10);
        when x"18"  => return to_signed(-104,10);
        when x"19"  => return to_signed(-103,10);
        when x"1a"  => return to_signed(-102,10);
        when x"1b"  => return to_signed(-101,10);
        when x"1c"  => return to_signed(-100,10);
        when x"1d"  => return to_signed(- 99,10);
        when x"1e"  => return to_signed(- 98,10);
        when x"1f"  => return to_signed(- 97,10);
        when x"20"  => return to_signed(- 96,10);
        when x"21"  => return to_signed(- 95,10);
        when x"22"  => return to_signed(- 94,10);
        when x"23"  => return to_signed(- 93,10);
        when x"24"  => return to_signed(- 92,10);
        when x"25"  => return to_signed(- 91,10);
        when x"26"  => return to_signed(- 80,10);
        when x"27"  => return to_signed(- 89,10);
        when x"28"  => return to_signed(- 88,10);
        when x"29"  => return to_signed(- 87,10);
        when x"2a"  => return to_signed(- 86,10);
        when x"2b"  => return to_signed(- 85,10);
        when x"2c"  => return to_signed(- 84,10);
        when x"2d"  => return to_signed(- 83,10);
        when x"2e"  => return to_signed(- 82,10);
        when x"2f"  => return to_signed(- 81,10);
        when x"30"  => return to_signed(- 80,10);
        when x"31"  => return to_signed(- 79,10);
        when x"32"  => return to_signed(- 78,10);
        when x"33"  => return to_signed(- 77,10);
        when x"34"  => return to_signed(- 76,10);
        when x"35"  => return to_signed(- 75,10);
        when x"36"  => return to_signed(- 74,10);
        when x"37"  => return to_signed(- 73,10);
        when x"38"  => return to_signed(- 72,10);
        when x"39"  => return to_signed(- 71,10);
        when x"3a"  => return to_signed(- 70,10);
        when x"3b"  => return to_signed(- 69,10);
        when x"3c"  => return to_signed(- 68,10);
        when x"3d"  => return to_signed(- 67,10);
        when x"3e"  => return to_signed(- 66,10);
        when x"3f"  => return to_signed(- 65,10);
        when x"40"  => return to_signed(- 64,10);
        when x"41"  => return to_signed(- 63,10);
        when x"42"  => return to_signed(- 62,10);
        when x"43"  => return to_signed(- 61,10);
        when x"44"  => return to_signed(- 60,10);
        when x"45"  => return to_signed(- 59,10);
        when x"46"  => return to_signed(- 58,10);
        when x"47"  => return to_signed(- 57,10);
        when x"48"  => return to_signed(- 56,10);
        when x"49"  => return to_signed(- 55,10);
        when x"4a"  => return to_signed(- 54,10);
        when x"4b"  => return to_signed(- 53,10);
        when x"4c"  => return to_signed(- 52,10);
        when x"4d"  => return to_signed(- 51,10);
        when x"4e"  => return to_signed(- 50,10);
        when x"4f"  => return to_signed(- 49,10);
        when x"50"  => return to_signed(- 48,10);
        when x"51"  => return to_signed(- 47,10);
        when x"52"  => return to_signed(- 46,10);
        when x"53"  => return to_signed(- 45,10);
        when x"54"  => return to_signed(- 44,10);
        when x"55"  => return to_signed(- 43,10);
        when x"56"  => return to_signed(- 42,10);
        when x"57"  => return to_signed(- 41,10);
        when x"58"  => return to_signed(- 40,10);
        when x"59"  => return to_signed(- 39,10);
        when x"5a"  => return to_signed(- 38,10);
        when x"5b"  => return to_signed(- 37,10);
        when x"5c"  => return to_signed(- 36,10);
        when x"5d"  => return to_signed(- 35,10);
        when x"5e"  => return to_signed(- 34,10);
        when x"5f"  => return to_signed(- 33,10);
        when x"60"  => return to_signed(- 32,10);
        when x"61"  => return to_signed(- 31,10);
        when x"62"  => return to_signed(- 30,10);
        when x"63"  => return to_signed(- 29,10);
        when x"64"  => return to_signed(- 28,10);
        when x"65"  => return to_signed(- 27,10);
        when x"66"  => return to_signed(- 26,10);
        when x"67"  => return to_signed(- 25,10);
        when x"68"  => return to_signed(- 24,10);
        when x"69"  => return to_signed(- 23,10);
        when x"6a"  => return to_signed(- 22,10);
        when x"6b"  => return to_signed(- 21,10);
        when x"6c"  => return to_signed(- 20,10);
        when x"6d"  => return to_signed(- 19,10);
        when x"6e"  => return to_signed(- 18,10);
        when x"6f"  => return to_signed(- 17,10);
        when x"70"  => return to_signed(- 16,10);
        when x"71"  => return to_signed(- 15,10);
        when x"72"  => return to_signed(- 14,10);
        when x"73"  => return to_signed(- 13,10);
        when x"74"  => return to_signed(- 12,10);
        when x"75"  => return to_signed(- 11,10);
        when x"76"  => return to_signed(- 10,10);
        when x"77"  => return to_signed(-  9,10);
        when x"78"  => return to_signed(-  8,10);
        when x"79"  => return to_signed(-  7,10);
        when x"7a"  => return to_signed(-  6,10);
        when x"7b"  => return to_signed(-  5,10);
        when x"7c"  => return to_signed(-  4,10);
        when x"7d"  => return to_signed(-  3,10);
        when x"7e"  => return to_signed(-  2,10);
        when x"7f"  => return to_signed(-  1,10);
        when x"80"  => return to_signed(   0,10);
        when x"81"  => return to_signed(   1,10);
        when x"82"  => return to_signed(   2,10);
        when x"83"  => return to_signed(   3,10);
        when x"84"  => return to_signed(   4,10);
        when x"85"  => return to_signed(   5,10);
        when x"86"  => return to_signed(   6,10);
        when x"87"  => return to_signed(   7,10);
        when x"88"  => return to_signed(   8,10);
        when x"89"  => return to_signed(   9,10);
        when x"8a"  => return to_signed(  10,10);
        when x"8b"  => return to_signed(  11,10);
        when x"8c"  => return to_signed(  12,10);
        when x"8d"  => return to_signed(  13,10);
        when x"8e"  => return to_signed(  14,10);
        when x"8f"  => return to_signed(  15,10);
        when x"90"  => return to_signed(  16,10);
        when x"91"  => return to_signed(  17,10);
        when x"92"  => return to_signed(  18,10);
        when x"93"  => return to_signed(  19,10);
        when x"94"  => return to_signed(  20,10);
        when x"95"  => return to_signed(  21,10);
        when x"96"  => return to_signed(  22,10);
        when x"97"  => return to_signed(  23,10);
        when x"98"  => return to_signed(  24,10);
        when x"99"  => return to_signed(  25,10);
        when x"9a"  => return to_signed(  26,10);
        when x"9b"  => return to_signed(  27,10);
        when x"9c"  => return to_signed(  28,10);
        when x"9d"  => return to_signed(  29,10);
        when x"9e"  => return to_signed(  30,10);
        when x"9f"  => return to_signed(  31,10);
        when x"a0"  => return to_signed(  32,10);
        when x"a1"  => return to_signed(  33,10);
        when x"a2"  => return to_signed(  34,10);
        when x"a3"  => return to_signed(  35,10);
        when x"a4"  => return to_signed(  36,10);
        when x"a5"  => return to_signed(  37,10);
        when x"a6"  => return to_signed(  38,10);
        when x"a7"  => return to_signed(  39,10);
        when x"a8"  => return to_signed(  40,10);
        when x"a9"  => return to_signed(  41,10);
        when x"aa"  => return to_signed(  42,10);
        when x"ab"  => return to_signed(  43,10);
        when x"ac"  => return to_signed(  44,10);
        when x"ad"  => return to_signed(  45,10);
        when x"ae"  => return to_signed(  46,10);
        when x"af"  => return to_signed(  47,10);
        when x"b0"  => return to_signed(  48,10);
        when x"b1"  => return to_signed(  49,10);
        when x"b2"  => return to_signed(  50,10);
        when x"b3"  => return to_signed(  51,10);
        when x"b4"  => return to_signed(  52,10);
        when x"b5"  => return to_signed(  53,10);
        when x"b6"  => return to_signed(  54,10);
        when x"b7"  => return to_signed(  55,10);
        when x"b8"  => return to_signed(  56,10);
        when x"b9"  => return to_signed(  57,10);
        when x"ba"  => return to_signed(  58,10);
        when x"bb"  => return to_signed(  59,10);
        when x"bc"  => return to_signed(  60,10);
        when x"bd"  => return to_signed(  61,10);
        when x"be"  => return to_signed(  62,10);
        when x"bf"  => return to_signed(  63,10);
        when x"c0"  => return to_signed(  64,10);
        when x"c1"  => return to_signed(  65,10);
        when x"c2"  => return to_signed(  66,10);
        when x"c3"  => return to_signed(  67,10);
        when x"c4"  => return to_signed(  68,10);
        when x"c5"  => return to_signed(  69,10);
        when x"c6"  => return to_signed(  70,10);
        when x"c7"  => return to_signed(  71,10);
        when x"c8"  => return to_signed(  72,10);
        when x"c9"  => return to_signed(  73,10);
        when x"ca"  => return to_signed(  74,10);
        when x"cb"  => return to_signed(  75,10);
        when x"cc"  => return to_signed(  76,10);
        when x"cd"  => return to_signed(  77,10);
        when x"ce"  => return to_signed(  78,10);
        when x"cf"  => return to_signed(  79,10);
        when x"d0"  => return to_signed(  80,10);
        when x"d1"  => return to_signed(  81,10);
        when x"d2"  => return to_signed(  82,10);
        when x"d3"  => return to_signed(  83,10);
        when x"d4"  => return to_signed(  84,10);
        when x"d5"  => return to_signed(  85,10);
        when x"d6"  => return to_signed(  86,10);
        when x"d7"  => return to_signed(  87,10);
        when x"d8"  => return to_signed(  88,10);
        when x"d9"  => return to_signed(  89,10);
        when x"da"  => return to_signed(  90,10);
        when x"db"  => return to_signed(  91,10);
        when x"dc"  => return to_signed(  92,10);
        when x"dd"  => return to_signed(  93,10);
        when x"de"  => return to_signed(  94,10);
        when x"df"  => return to_signed(  95,10);
        when x"e0"  => return to_signed(  96,10);
        when x"e1"  => return to_signed(  97,10);
        when x"e2"  => return to_signed(  98,10);
        when x"e3"  => return to_signed(  99,10);
        when x"e4"  => return to_signed( 100,10);
        when x"e5"  => return to_signed( 101,10);
        when x"e6"  => return to_signed( 102,10);
        when x"e7"  => return to_signed( 103,10);
        when x"e8"  => return to_signed( 104,10);
        when x"e9"  => return to_signed( 105,10);
        when x"ea"  => return to_signed( 106,10);
        when x"eb"  => return to_signed( 107,10);
        when x"ec"  => return to_signed( 108,10);
        when x"ed"  => return to_signed( 109,10);
        when x"ee"  => return to_signed( 110,10);
        when x"ef"  => return to_signed( 111,10);
        when x"f0"  => return to_signed( 112,10);
        when x"f1"  => return to_signed( 113,10);
        when x"f2"  => return to_signed( 114,10);
        when x"f3"  => return to_signed( 115,10);
        when x"f4"  => return to_signed( 116,10);
        when x"f5"  => return to_signed( 117,10);
        when x"f6"  => return to_signed( 118,10);
        when x"f7"  => return to_signed( 119,10);
        when x"f8"  => return to_signed( 120,10);
        when x"f9"  => return to_signed( 121,10);
        when x"fa"  => return to_signed( 122,10);
        when x"fb"  => return to_signed( 123,10);
        when x"fc"  => return to_signed( 124,10);
        when x"fd"  => return to_signed( 125,10);
        when x"fe"  => return to_signed( 126,10);
        when x"ff"  => return to_signed( 127,10);
        when others => return to_signed(   0,10);
    end case;
end u8tos10;

signal vmix_reg : s10;
signal vmix_m16 : word;

begin

r0w1 <= not r1w0;
clk1m <= ph2;

v1_freq <= u8(sid_regs(1)) & u8(sid_regs(0));
v1_pw   <= u4(sid_regs(3)(3 downto 0)) & u8(sid_regs(2));

v2_freq <= u8(sid_regs(8)) & u8(sid_regs(7));
v2_pw   <= u4(sid_regs(10)(3 downto 0)) & u8(sid_regs(9));

v3_freq <= u8(sid_regs(15)) & u8(sid_regs(14));
v3_pw   <= u4(sid_regs(17)(3 downto 0)) & u8(sid_regs(16));

filter_fc <= u8(sid_regs(22)) & u3(sid_regs(21)(2 downto 0));

voice_1: sid_voice port map (
    clk1m       => clk1m,
    freq        => v1_freq,
    pw          => v1_pw,
    wvfm        => v1_wvfm,
    test        => v1_test,
    rmod        => v1_ring,
    sync        => v1_sync,
    gate        => v1_gate,
    enva        => u4(v1_enva),
    envd        => u4(v1_envd),
    envs        => u4(v1_envs),
    envr        => u4(v1_envr),
    envo        => v1_envo,
    osco        => v1_osco,
    uout        => v1_out
); 

voice_2: sid_voice port map (
    clk1m       => clk1m,
    freq        => v2_freq,
    pw          => v2_pw,
    wvfm        => v2_wvfm,
    test        => v2_test,
    rmod        => v2_ring,
    sync        => v2_sync,
    gate        => v2_gate,
    enva        => u4(v2_enva),
    envd        => u4(v2_envd),
    envs        => u4(v2_envs),
    envr        => u4(v2_envr),
    envo        => v2_envo,
    osco        => v2_osco,
    uout        => v2_out
); 

voice_3: sid_voice port map (
    clk1m       => clk1m,
    freq        => v3_freq,
    pw          => v3_pw,
    wvfm        => v3_wvfm,
    test        => v3_test,
    rmod        => v3_ring,
    sync        => v3_sync,
    gate        => v3_gate,
    enva        => u4(v3_enva),
    envd        => u4(v3_envd),
    envs        => u4(v3_envs),
    envr        => u4(v3_envr),
    envo        => v3_envo,
    osco        => v3_osco,
    uout        => v3_out
);

hold_mix: process(clk1M, mute3, v1_out, v2_out, v3_out) is
variable v1s    : s10;
variable v2s    : s10;
variable v3s    : s10;
variable vpair  : s10;
begin
    --v1s := s10("00" & v1_out) - 128;
    --v2s := s10("00" & v2_out) - 128;
    --v3s := s10("00" & v3_out) - 128;
    v1s     := u8tos10(v1_out);
    v2s     := u8tos10(v2_out);
    v3s     := u8tos10(v3_out);
    vpair   := v1s + v2s;
    if (rising_edge(clk1M)) then
        if (mute3 = '1') then
            vmix_reg <= vpair;
        else
            vmix_reg <= vpair + v3s;
        end if;
    end if;
end process hold_mix;

scaler: ccm_85 port map(
    clk     => clk1M,
    a       => slv10(vmix_reg),
    p       => vmix_m16
);

audio: process(clk1m, vmix_m16) is
--variable s20tmp : s20;
begin
    --s20tmp := vmix_reg*85;
    if (rising_edge(clk1m)) then
        --s16audio <= s20tmp(15 downto 0);
        s16audio <= s16(vmix_m16);
    end if;
end process audio;

reg_rd: process(ph2, rga, sid_regs, v3_osco, v3_envo) is
variable rdata : byte;
begin
    case "000" & rga is
        when x"19"      => rdata := sid_regs(25);
        when x"1a"      => rdata := sid_regs(26);
        when x"1b"      => rdata := byte(v3_osco);
        when x"1c"      => rdata := byte(v3_envo(7 downto 0));
        when others     => rdata := "11111111";
    end case;
    if (rising_edge(ph2)) then
        dout <= rdata;
    end if;
end process reg_rd;

reg_wr: process(ph2, rga, r1w0, din) is
begin
    if (falling_edge(ph2)) then
        if ((("000"&rga) < x"19") and (r0w1 = '1')) then
            sid_regs(to_integer(unsigned(rga))) <= din;
        end if;
    end if;    
end process reg_wr;

end sid_impl;
